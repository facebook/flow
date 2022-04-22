(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is the entry point of the typechecker. It sets up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

open Utils_js

let choose_provider_and_warn_about_duplicates =
  let warn_duplicate_providers m (provider, _) duplicates acc =
    match duplicates with
    | [] -> acc
    | (f, _) :: fs -> SMap.add m (provider, (f, List.map fst fs)) acc
  in
  fun m errmap providers fallback ->
    let (definitions, implementations) =
      let f (key, _) = Files.has_flow_ext key in
      List.partition f providers
    in
    match (implementations, definitions) with
    (* If there are no definitions or implementations, use the fallback *)
    | ([], []) -> (fallback (), errmap)
    (* Else if there are no definitions, use the first implementation *)
    | (impl :: dup_impls, []) -> (Some (snd impl), warn_duplicate_providers m impl dup_impls errmap)
    (* Else use the first definition *)
    | ([], defn :: dup_defns) -> (Some (snd defn), warn_duplicate_providers m defn dup_defns errmap)
    (* Don't complain about the first implementation being a duplicate *)
    | (_impl :: dup_impls, defn :: dup_defns) ->
      let errmap =
        errmap
        |> warn_duplicate_providers m defn dup_impls
        |> warn_duplicate_providers m defn dup_defns
      in
      (Some (snd defn), errmap)

(**
 * A set of module.name_mapper config entry allows users to specify regexp
 * matcher strings each with a template string in order to map the names of a
 * dependency in a JS file to another name before trying to resolve it.
 *
 * The user can specify any number of these mappers, but the one that gets
 * applied to any given module name is the first one whose name matches the
 * regexp string. For the node module system, we go a step further and only
 * choose candidates that match the string *and* are a valid, resolvable path.
 *)
let module_name_candidates_cache = Hashtbl.create 50

let module_name_candidates ~options name =
  match Hashtbl.find_opt module_name_candidates_cache name with
  | Some candidates -> candidates
  | None ->
    let mappers = Options.module_name_mappers options in
    let root = Options.root options in
    let expand_project_root_token = Files.expand_project_root_token ~root in
    let map_name mapped_names (regexp, template) =
      let new_name =
        name
        (* First we apply the mapper *)
        |> Str.global_replace regexp template
        (* Then we replace the PROJECT_ROOT placeholder. *)
        |> expand_project_root_token
      in
      if new_name = name then
        mapped_names
      else
        new_name :: mapped_names
    in
    let candidates = List.rev (name :: List.fold_left map_name [] mappers) in
    Hashtbl.add module_name_candidates_cache name candidates;
    candidates

let add_package filename = function
  | Ok package -> Package_heaps.Package_heap_mutator.add_package_json filename package
  | Error _ -> Package_heaps.Package_heap_mutator.add_error filename

type package_incompatible_reason =
  (* Didn't exist before, now it exists *)
  | New
  (* Was valid, now is invalid *)
  | Became_invalid
  (* Was invalid, now is valid *)
  | Became_valid
  (* The `name` property changed from the former to the latter *)
  | Name_changed of string option * string option
  (* The `main` property changed from the former to the latter *)
  | Main_changed of string option * string option
  | Unknown

let string_of_package_incompatible_reason =
  let string_of_option = function
    | None -> "<None>"
    | Some x -> x
  in
  function
  | New -> "new"
  | Became_invalid -> "became invalid"
  | Became_valid -> "became valid"
  | Name_changed (old, new_) ->
    Printf.sprintf "name changed from `%s` to `%s`" (string_of_option old) (string_of_option new_)
  | Main_changed (old, new_) ->
    Printf.sprintf "main changed from `%s` to `%s`" (string_of_option old) (string_of_option new_)
  | Unknown -> "Unknown"

type package_incompatible_return =
  | Compatible
  | Incompatible of package_incompatible_reason

let package_incompatible ~reader filename new_package =
  let old_package = Package_heaps.Reader.get_package ~reader filename in
  match (old_package, new_package) with
  | (None, Ok _) -> Incompatible New (* didn't exist before, found a new one *)
  | (None, Error _) -> Compatible (* didn't exist before, new one is invalid *)
  | (Some (Error ()), Error _) -> Compatible (* was invalid before, still invalid *)
  | (Some (Error ()), Ok _) -> Incompatible Became_valid (* was invalid before, new one is valid *)
  | (Some (Ok _), Error _) -> Incompatible Became_invalid (* existed before, new one is invalid *)
  | (Some (Ok old_package), Ok new_package) ->
    if old_package = new_package then
      Compatible
    else
      let old_main = Package_json.main old_package in
      let new_main = Package_json.main new_package in
      let old_name = Package_json.name old_package in
      let new_name = Package_json.name new_package in
      if old_name <> new_name then
        Incompatible (Name_changed (old_name, new_name))
      else if old_main <> new_main then
        Incompatible (Main_changed (old_main, new_main))
      else
        (* This shouldn't happen -- if it does, it probably means we need to add cases above *)
        Incompatible Unknown

type resolution_acc = { mutable paths: SSet.t }

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and docblock info, make the name of the module it exports. *)
  val exported_module : Options.t -> File_key.t -> Docblock.t -> string option

  (* Given a file and a reference in it to an imported module, make the name of
     the module it refers to. If given an optional reference to an accumulator,
     record paths that were looked up but not found during resolution. *)
  val imported_module :
    options:Options.t ->
    reader:Abstract_state_reader.t ->
    SSet.t SMap.t ->
    File_key.t ->
    ?resolution_acc:resolution_acc ->
    string ->
    Modulename.t

  (* for a given module name, choose a provider from among a set of
     files with that exported name. also check for duplicates and
     generate warnings, as dictated by module system rules. *)
  val choose_provider :
    (* module name *)
    string ->
    (* set of candidate provider files *)
    (File_key.t * Parsing_heaps.file_addr) list ->
    (* map from files to error sets (accumulator) *)
    (File_key.t * File_key.t Nel.t) SMap.t ->
    (* file, error map (accumulator) *)
    Parsing_heaps.file_addr option * (File_key.t * File_key.t Nel.t) SMap.t
end

(****************** Node module system *********************)

(* TODO this exists only until we start resolving files using
   NameHeap. unfortunately that will require more refactoring
   than it should, since imported_module is currently called
   during local inference, and simply storing raw module names
   in cx.required et al and looking them up at merge time appears
   to violate some well-hidden private agreements. TODO *)

(* only purpose here is to guarantee a case-sensitive file exists
   and try to keep it from being too horrendously expensive *)

let case_sensitive = not (Sys.file_exists (String.uppercase_ascii (Sys.getcwd ())))

(* map of dirs to file lists *)

(** TODO [perf]: investigate whether this takes too much memory **)
let files_in_dir = ref SMap.empty

(* called from Types_js.typecheck, so we rebuild every time *)
let clear_filename_cache () = files_in_dir := SMap.empty

(* when system is case-insensitive, do our own file exists check *)
let rec file_exists path =
  (* case doesn't matter for "/", ".", "..." and these serve as a base-case for
   * case-insensitive filesystems *)
  let dir = Filename.dirname path in
  if
    case_sensitive
    || path = Filename.current_dir_name
    || path = Filename.parent_dir_name
    || path = dir
  then
    Sys.file_exists path
  else
    let files =
      match SMap.find_opt dir !files_in_dir with
      | Some files -> files
      | None ->
        let files =
          if Disk.is_directory dir && file_exists dir then
            SSet.of_list (Array.to_list (Sys.readdir dir))
          else
            SSet.empty
        in
        files_in_dir := SMap.add dir files !files_in_dir;
        files
    in
    SSet.mem (Filename.basename path) files

(*******************************)

module Node = struct
  let exported_module _ _ _ = None

  let record_path path = function
    | None -> ()
    | Some resolution_acc -> resolution_acc.paths <- SSet.add path resolution_acc.paths

  (** [path_if_exists acc path] determines whether [path] (a) has an extension
    Flow cares about, (b) exists, and (c) is not ignored. If [path] does not
    exist, it checks whether [path ^ ".flow"] exists, is not ignored, etc.
    Returns [Some path] if so; if not, adds [path] to [acc] and returns [None].

    Note: if [path ^ ".flow"] exists and [path] does not, returns [Some path],
    not [Some (path ^ ".flow")]! *)
  let path_if_exists =
    let path_exists ~file_options path =
      file_exists path && (not (Files.is_ignored file_options path)) && not (Sys.is_directory path)
    in
    let is_flow_file ~file_options path = Files.is_flow_file ~options:file_options path in
    fun ~file_options resolution_acc raw_path ->
      let (path, path_to_check) =
        match Sys_utils.realpath raw_path with
        | Some path ->
          let is_flow = is_flow_file ~file_options path in
          (path, Base.Option.some_if is_flow path)
        | None ->
          let decl_path = raw_path ^ Files.flow_ext in
          let is_flow = is_flow_file ~file_options raw_path && Sys.file_exists decl_path in
          (raw_path, Base.Option.some_if is_flow decl_path)
      in
      if Base.Option.exists ~f:(path_exists ~file_options) path_to_check then
        Some (Files.eponymous_module (Files.filename_from_string ~options:file_options path))
      else (
        record_path path resolution_acc;
        None
      )

  let path_if_exists_with_file_exts ~file_options resolution_acc path file_exts =
    lazy_seq
      (file_exts
      |> Base.List.map ~f:(fun ext ->
             lazy (path_if_exists ~file_options resolution_acc (path ^ ext))
         )
      )

  let parse_main ~reader ~file_options resolution_acc package_filename file_exts =
    let%bind.Base.Option package_filename = Sys_utils.realpath package_filename in
    let package =
      match Package_heaps.Reader_dispatcher.get_package ~reader package_filename with
      | Some (Ok package) -> package
      | Some (Error ()) ->
        (* invalid, but we already raised an error when building PackageHeap *)
        Package_json.empty
      | None -> Package_json.empty
    in
    match Package_json.main package with
    | None -> None
    | Some file ->
      let dir = Filename.dirname package_filename in
      let path = Files.normalize_path dir file in
      let path_w_index = Filename.concat path "index" in
      lazy_seq
        [
          lazy (path_if_exists ~file_options resolution_acc path);
          lazy (path_if_exists_with_file_exts ~file_options resolution_acc path file_exts);
          lazy (path_if_exists_with_file_exts ~file_options resolution_acc path_w_index file_exts);
        ]

  let resolve_relative ~options ~reader ?resolution_acc root_path rel_path =
    let file_options = Options.file_options options in
    let path = Files.normalize_path root_path rel_path in
    (* We do not try resource file extensions here. So while you can write
     * require('foo') to require foo.js, it should never resolve to foo.css
     *)
    let file_exts = Files.module_file_exts file_options in
    lazy_seq
      [
        lazy (path_if_exists ~file_options resolution_acc path);
        lazy (path_if_exists_with_file_exts ~file_options resolution_acc path file_exts);
        lazy
          (parse_main
             ~reader
             ~file_options
             resolution_acc
             (Filename.concat path "package.json")
             file_exts
          );
        lazy
          (path_if_exists_with_file_exts
             ~file_options
             resolution_acc
             (Filename.concat path "index")
             file_exts
          );
      ]

  let rec node_module ~options ~reader node_modules_containers file resolution_acc dir r =
    let file_options = Options.file_options options in
    lazy_seq
      [
        lazy
          (match SMap.find_opt dir node_modules_containers with
          | Some existing_node_modules_dirs ->
            lazy_seq
              (Files.node_resolver_dirnames file_options
              |> Base.List.map ~f:(fun dirname ->
                     lazy
                       ( if SSet.mem dirname existing_node_modules_dirs then
                         resolve_relative
                           ~options
                           ~reader
                           ?resolution_acc
                           dir
                           (spf "%s%s%s" dirname Filename.dir_sep r)
                       else
                         None
                       )
                 )
              )
          | None -> None);
        lazy
          (let parent_dir = Filename.dirname dir in
           if dir = parent_dir then
             None
           else
             node_module
               ~options
               ~reader
               node_modules_containers
               file
               resolution_acc
               (Filename.dirname dir)
               r
          );
      ]

  let absolute r = Str.string_match Files.absolute_path_regexp r 0

  let explicitly_relative r =
    Str.string_match Files.current_dir_name r 0 || Str.string_match Files.parent_dir_name r 0

  let resolve_import ~options ~reader node_modules_containers f ?resolution_acc import_str =
    let file = File_key.to_string f in
    let dir = Filename.dirname file in
    let root_str = Options.root options |> Path.to_string in
    if explicitly_relative import_str || absolute import_str then
      resolve_relative ~options ~reader ?resolution_acc dir import_str
    else
      lazy_seq
        [
          lazy
            ( if Options.node_resolver_allow_root_relative options then
              lazy_seq
                (Options.node_resolver_root_relative_dirnames options
                |> Base.List.map ~f:(fun root_relative_dirname ->
                       lazy
                         (let root_str =
                            if root_relative_dirname = "" then
                              root_str
                            else
                              Filename.concat root_str root_relative_dirname
                          in
                          resolve_relative ~options ~reader ?resolution_acc root_str import_str
                         )
                   )
                )
            else
              None
            );
          lazy (node_module ~options ~reader node_modules_containers f resolution_acc dir import_str);
        ]

  let imported_module ~options ~reader node_modules_containers file ?resolution_acc r =
    match
      List.find_map
        (resolve_import ~options ~reader node_modules_containers file ?resolution_acc)
        (module_name_candidates ~options r)
    with
    | Some m -> m
    | None -> Modulename.String r

  (* in node, file names are module names, as guaranteed by
     our implementation of exported_name, so anything but a
     singleton provider set is craziness. *)
  let choose_provider m files errmap =
    let fallback () = None in
    choose_provider_and_warn_about_duplicates m errmap files fallback
end

(****************** Haste module system *********************)

module Haste : MODULE_SYSTEM = struct
  let short_module_name_of = function
    | File_key.Builtins -> assert false
    | File_key.LibFile file
    | File_key.SourceFile file
    | File_key.JsonFile file
    | File_key.ResourceFile file ->
      Filename.basename file |> Filename.chop_extension

  let is_mock =
    let mock_path = Str.regexp ".*/__mocks__/.*" in
    function
    | File_key.Builtins -> false
    | File_key.LibFile file
    | File_key.SourceFile file
    | File_key.JsonFile file
    | File_key.ResourceFile file ->
      (* Standardize \ to / in path for Windows *)
      let file = Sys_utils.normalize_filename_dir_sep file in
      Str.string_match mock_path file 0

  let is_haste_file options =
    let includes = lazy (Base.List.map ~f:Str.regexp (Options.haste_paths_includes options)) in
    let excludes = lazy (Base.List.map ~f:Str.regexp (Options.haste_paths_excludes options)) in
    let matches_includes name =
      List.exists (fun r -> Str.string_match r name 0) (Lazy.force includes)
    in
    let matches_excludes name =
      List.exists (fun r -> Str.string_match r name 0) (Lazy.force excludes)
    in
    (fun name -> matches_includes name && not (matches_excludes name))

  let haste_name =
    let reduce_name name (regexp, template) = Str.global_replace regexp template name in
    (fun options name -> List.fold_left reduce_name name (Options.haste_name_reducers options))

  let exported_module options =
    let is_haste_file = is_haste_file options in
    fun file info ->
      match file with
      | File_key.SourceFile _ ->
        if is_mock file then
          Some (short_module_name_of file)
        else if Options.haste_use_name_reducers options then
          (* Standardize \ to / in path for Windows *)
          let normalized_file_name =
            Sys_utils.normalize_filename_dir_sep (File_key.to_string file)
          in
          if is_haste_file normalized_file_name then
            Some (haste_name options normalized_file_name)
          else
            None
        else
          Docblock.providesModule info
      | _ ->
        (* Lib files, resource files, etc don't have any fancy haste name *)
        None

  let resolve_haste_module r =
    match Parsing_heaps.get_haste_module r with
    | Some _ -> Some (Modulename.String r)
    | None -> None

  let resolve_haste_package ~options ~reader file ?resolution_acc r =
    let (dir_opt, rest) =
      match Str.split_delim (Str.regexp_string "/") r with
      | [] -> (None, [])
      | package :: rest ->
        (Package_heaps.Reader_dispatcher.get_package_directory ~reader package, rest)
    in
    match dir_opt with
    | None -> None
    | Some package_dir ->
      let file_dirname = Filename.dirname (File_key.to_string file) in
      Files.construct_path package_dir rest
      |> Node.resolve_relative ~options ~reader ?resolution_acc file_dirname

  let resolve_import ~options ~reader node_modules_containers file ?resolution_acc r =
    lazy_seq
      [
        lazy (resolve_haste_module r);
        lazy (resolve_haste_package ~options ~reader file ?resolution_acc r);
        lazy (Node.resolve_import ~options ~reader node_modules_containers file ?resolution_acc r);
      ]

  let imported_module ~options ~reader node_modules_containers file ?resolution_acc r =
    (* For historical reasons, the Haste module system always picks the first
     * matching candidate, unlike the Node module system which picks the first
     * "valid" matching candidate. *)
    let r = List.hd (module_name_candidates ~options r) in
    match resolve_import ~options ~reader node_modules_containers file ?resolution_acc r with
    | Some m -> m
    | None -> Modulename.String r

  (* in haste, many files may provide the same module. here we're also
     supporting the notion of mock modules - allowed duplicates used as
     fallbacks. we prefer the non-mock if it exists, otherwise choose an
     arbitrary mock, if any exist. if multiple non-mock providers exist,
     we pick one arbitrarily and issue duplicate module warnings for the
     rest. *)
  let choose_provider m files errmap =
    match files with
    | [] -> (None, errmap)
    | [(_, p)] -> (Some p, errmap)
    | files ->
      let (mocks, non_mocks) =
        let f (key, _) = is_mock key in
        List.partition f files
      in
      let fallback () = Some (snd (List.hd mocks)) in
      choose_provider_and_warn_about_duplicates m errmap non_mocks fallback
end

(****************** module system switch *********************)

(* Switch between module systems, based on environment. We could eventually use
   functors, but that seems like overkill at this point. *)

let module_system = ref None

(* TODO: is it premature optimization to memoize this? how bad is doing the
   Hashtbl.find each time? *)
let get_module_system opts =
  match !module_system with
  | Some system -> system
  | None ->
    let module M =
    ( val match Options.module_system opts with
          | Options.Node -> (module Node : MODULE_SYSTEM)
          | Options.Haste -> (module Haste : MODULE_SYSTEM)
      )
    in
    let system = (module M : MODULE_SYSTEM) in
    module_system := Some system;
    system

let exported_module ~options =
  let module M = (val get_module_system options) in
  M.exported_module options

let imported_module ~options ~reader ~node_modules_containers file ?resolution_acc r =
  let module M = (val get_module_system options) in
  M.imported_module ~options ~reader node_modules_containers file ?resolution_acc r

let choose_provider ~options m files errmap =
  let module M = (val get_module_system options) in
  M.choose_provider m files errmap

(******************)
(***** public *****)
(******************)

(** Resolve references to required modules in a file, and record the results.

    TODO [perf]: measure size and possibly optimize *)
let resolved_requires_of ~options ~reader node_modules_containers file require_loc =
  let resolution_acc = { paths = SSet.empty } in
  let resolved_modules =
    SMap.fold
      (fun mref _locs acc ->
        let m =
          imported_module file mref ~options ~reader ~node_modules_containers ~resolution_acc
        in
        SMap.add mref m acc)
      require_loc
      SMap.empty
  in
  let { paths = phantom_dependencies } = resolution_acc in
  Parsing_heaps.mk_resolved_requires ~resolved_modules ~phantom_dependencies

let add_parsed_resolved_requires ~mutator ~reader ~options ~node_modules_containers file =
  let file_addr = Parsing_heaps.get_file_addr_unsafe file in
  let parse = Parsing_heaps.Mutator_reader.get_typed_parse_unsafe ~reader file file_addr in
  let file_sig = Parsing_heaps.read_file_sig_unsafe file parse |> File_sig.abstractify_locs in
  let require_loc = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
  let resolved_requires =
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    resolved_requires_of ~options ~reader node_modules_containers file require_loc
  in
  Parsing_heaps.Resolved_requires_mutator.add_resolved_requires
    mutator
    file_addr
    parse
    resolved_requires

(* Repick providers for modules that are exported by new and changed files, or
   were provided by changed and deleted files.

   For deleted files, their exported modules, if in old modules, will pick a
   new provider, or be left with no provider.

   For changed files, their exported modules, if in old modules, may pick
   the same provider (i.e., the changed file) or a new provider (a different
   file). If not in old modules, they may pick a new provider (i.e., the
   changed file) or the same provider (a different file).

   For new files, their exported modules may pick a new provider (i.e., the new
   file) or the same provider (a different file).

   Suppose that:
   new_or_changed is a list of parsed / unparsed file names.
   old_modules is a set of removed module names.

   Modules provided by parsed / unparsed files may or may not have a
   provider. Modules named in old_modules definitely do not have a
   provider. Together, they are considered "dirty" modules. Providers for dirty
   modules must be repicked.

   Files that depend on the subset of dirty modules that either have changed
   providers or are provided by changed files will be rechecked.

   Preconditions:
   1. all files in new_or_changed have entries in InfoHeap (true if
   we're properly calling add_parsed_info and add_unparsed_info for every
   parsed / unparsed file before calling commit_modules)
   2. all modules not mentioned in old_modules, but provided by one or more
   files in InfoHeap, have some provider registered in NameHeap.
   (However, the current provider may not be the one we now want,
   given newly parsed / unparsed files.)
   3. conversely all modules in old_modules lack a provider in NameHeap.

   Postconditions:
   1. all modules provided by at least 1 file in InfoHeap have a provider
   registered in NameHeap, and it's the provider we want according to our
   precedence and scoping rules.

   We make use of a shadow map in the master process which maintains
   a view of what's going on in NameHeap and InfoHeap, mapping module
   names to sets of filenames of providers.
   TODO: this shadow map is probably a perf bottleneck, get rid of it.

   Algorithm here:

   1. Calculate repick set:
   (a) add all removed modules to the set of modules to repick a provider for.
   (b) add the modules provided by all parsed / unparsed files to the repick set.

   2. Commit providers for dirty modules:
   (a) For each module in the repick set, pick a winner from its available
   providers. if it's different than the current provider, or if there is no
   current provider, add the new provider to the list to be registered.
   (b) remove the unregistered modules from NameHeap
   (c) register the new providers in NameHeap
*)
let commit_modules ~transaction ~workers ~options dirty_modules =
  let module Heap = SharedMem.NewAPI in
  let debug = Options.is_debug_mode options in
  let mutator = Parsing_heaps.Commit_modules_mutator.create transaction in
  let f (unchanged, no_providers, errmap) mname =
    let mname_str = Modulename.to_string mname in
    let (provider_ent, all_providers) =
      match mname with
      | Modulename.String name ->
        let m = Parsing_heaps.get_haste_module_unsafe name in
        (Heap.get_haste_provider m, Heap.get_haste_all_providers_exclusive m)
      | Modulename.Filename key ->
        let m = Parsing_heaps.get_file_module_unsafe key in
        (Heap.get_file_provider m, Heap.get_file_all_providers_exclusive m)
    in
    let all_providers =
      let f acc f =
        let key = Parsing_heaps.read_file_key f in
        FilenameMap.add key f acc
      in
      List.fold_left f FilenameMap.empty all_providers |> FilenameMap.bindings
    in
    let old_provider = Heap.entity_read_latest provider_ent in
    let (new_provider, errmap) = choose_provider ~options mname_str all_providers errmap in
    match (old_provider, new_provider) with
    | (_, None) ->
      if debug then prerr_endlinef "no remaining providers: %s" mname_str;
      Heap.entity_advance provider_ent None;
      let no_providers = Modulename.Set.add mname no_providers in
      (unchanged, no_providers, errmap)
    | (None, Some p) ->
      (* When can this happen? Either m pointed to a file that used to
         provide m and changed or got deleted (causing m to be in
         old_modules), or m didn't have a provider before. *)
      if debug then
        prerr_endlinef "initial provider %s -> %s" mname_str (Parsing_heaps.read_file_name p);
      Heap.entity_advance provider_ent (Some p);
      (unchanged, no_providers, errmap)
    | (Some old_p, Some new_p) ->
      if Heap.files_equal old_p new_p then (
        (* When can this happen? Say m pointed to f before, a different file
           f' that provides m changed (so m is not in old_modules), but f
           continues to be the chosen provider = p (winning over f'). *)
        if debug then
          prerr_endlinef
            "unchanged provider: %s -> %s"
            mname_str
            (Parsing_heaps.read_file_name new_p);
        let unchanged =
          if Heap.file_changed old_p then
            unchanged
          else
            Modulename.Set.add mname unchanged
        in
        (unchanged, no_providers, errmap)
      ) else (
        (* When can this happen? Say m pointed to f before, a different file
           f' that provides m changed (so m is not in old_modules), and
           now f' becomes the chosen provider = p (winning over f). *)
        if debug then
          prerr_endlinef
            "new provider: %s -> %s replaces %s"
            mname_str
            (Parsing_heaps.read_file_name new_p)
            (Parsing_heaps.read_file_name old_p);
        Heap.entity_advance provider_ent (Some new_p);
        (unchanged, no_providers, errmap)
      )
  in
  let%lwt (unchanged, no_providers, duplicate_providers) =
    MultiWorkerLwt.call
      workers
      ~job:(List.fold_left f)
      ~neutral:(Modulename.Set.empty, Modulename.Set.empty, SMap.empty)
      ~merge:(fun (a1, a2, a3) (b1, b2, b3) ->
        (Modulename.Set.union a1 b1, Modulename.Set.union a2 b2, SMap.union a3 b3))
      ~next:(MultiWorkerLwt.next workers (Modulename.Set.elements dirty_modules))
  in
  Parsing_heaps.Commit_modules_mutator.record_no_providers mutator no_providers;
  let changed_modules = Modulename.Set.diff dirty_modules unchanged in
  if debug then prerr_endlinef "*** done committing modules ***";
  Lwt.return (changed_modules, duplicate_providers)
