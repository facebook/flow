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

exception
  Failed_to_resolve_module of {
    name: string;
    file: File_key.t;
    exn: Exception.t;
  }

let () =
  Exception.register_printer (function
      | Failed_to_resolve_module { name; file; exn } ->
        Some
          (Printf.sprintf
             "Unexpected exception resolving module %S in %s: %s\n%s"
             name
             (File_key.to_string file)
             (Exception.get_ctor_string exn)
             (Exception.get_backtrace_string exn)
          )
      | _ -> None
      )

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
    let candidates = Nel.rev (name, List.fold_left map_name [] mappers) in
    Hashtbl.add module_name_candidates_cache name candidates;
    candidates

type package_incompatible_reason =
  | New  (** Didn't exist before, now it exists *)
  | Became_invalid  (** Was valid, now is invalid *)
  | Became_valid  (** Was invalid, now is valid *)
  | Name_changed of string option * string option
      (** The `name` property changed from the former to the latter *)
  | Main_changed of string option * string option
      (** The `main` property changed from the former to the latter *)
  | Haste_commonjs_changed of bool  (** The `haste_commonjs` property changed to this value *)
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
  | Haste_commonjs_changed new_ ->
    Printf.sprintf "haste_commonjs changed from `%b` to `%b`" (not new_) new_
  | Unknown -> "Unknown"

type package_incompatible_return =
  | Compatible
  | Incompatible of package_incompatible_reason

let package_incompatible ~reader filename new_package =
  let old_package = Parsing_heaps.Reader.get_package_info ~reader filename in
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
      let old_haste_commonjs = Package_json.haste_commonjs old_package in
      let new_haste_commonjs = Package_json.haste_commonjs new_package in
      if old_name <> new_name then
        Incompatible (Name_changed (old_name, new_name))
      else if old_main <> new_main then
        Incompatible (Main_changed (old_main, new_main))
      else if old_haste_commonjs <> new_haste_commonjs then
        Incompatible (Haste_commonjs_changed new_haste_commonjs)
      else
        (* This shouldn't happen -- if it does, it probably means we need to add cases above *)
        Incompatible Unknown

type phantom_acc = Modulename.Set.t ref

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and docblock info, make the name of the module it exports. *)
  val exported_module :
    Options.t ->
    File_key.t ->
    [ `Module of Docblock.t | `Package of Package_json.t ] ->
    string option

  (* Given a file and a reference in it to an imported module, make the name of
     the module it refers to. If given an optional reference to an accumulator,
     record paths that were looked up but not found during resolution. *)
  val imported_module :
    options:Options.t ->
    reader:Abstract_state_reader.t ->
    SSet.t SMap.t ->
    File_key.t ->
    ?phantom_acc:phantom_acc ->
    string ->
    Parsing_heaps.resolved_module

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

let is_relative_or_absolute r =
  Str.string_match Files.current_dir_name r 0
  || Str.string_match Files.parent_dir_name r 0
  || Str.string_match Files.absolute_path_regexp r 0

let resolve_symlinks path = Path.to_string (Path.make path)

let record_phantom_dependency mname = function
  | None -> ()
  | Some phantom_acc -> phantom_acc := Modulename.Set.add mname !phantom_acc

(****************** Node module system *********************)

module Node = struct
  let exported_module _ _ _ = None

  let path_if_exists ~reader ~file_options phantom_acc path =
    let path = resolve_symlinks path in
    let mname = Files.eponymous_module (Files.filename_from_string ~options:file_options path) in
    match Parsing_heaps.Reader_dispatcher.get_provider ~reader mname with
    | Some _ -> Some mname
    | None ->
      record_phantom_dependency mname phantom_acc;
      None

  let path_if_exists_with_file_exts ~reader ~file_options phantom_acc path file_exts =
    let f ext = path_if_exists ~reader ~file_options phantom_acc (path ^ ext) in
    List.find_map f file_exts

  let parse_main ~reader ~file_options phantom_acc package_filename file_exts =
    let package_filename = resolve_symlinks package_filename in
    let package =
      let file_key = File_key.JsonFile package_filename in
      match Parsing_heaps.Reader_dispatcher.get_package_info ~reader file_key with
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
          lazy (path_if_exists ~reader ~file_options phantom_acc path);
          lazy (path_if_exists_with_file_exts ~reader ~file_options phantom_acc path file_exts);
          lazy
            (path_if_exists_with_file_exts ~reader ~file_options phantom_acc path_w_index file_exts);
        ]

  let resolve_package ~options ~reader ?phantom_acc package_dir =
    let file_options = Options.file_options options in
    let file_exts = Files.module_file_exts file_options in
    lazy_seq
      [
        lazy
          (parse_main
             ~reader
             ~file_options
             phantom_acc
             (Filename.concat package_dir "package.json")
             file_exts
          );
        lazy
          (path_if_exists_with_file_exts
             ~reader
             ~file_options
             phantom_acc
             (Filename.concat package_dir "index")
             file_exts
          );
      ]

  let resolve_relative ~options ~reader ?phantom_acc root_path rel_path =
    let file_options = Options.file_options options in
    let path = Files.normalize_path root_path rel_path in
    (* We do not try resource file extensions here. So while you can write
     * require('foo') to require foo.js, it should never resolve to foo.css
     *)
    let file_exts = Files.module_file_exts file_options in
    lazy_seq
      [
        lazy (path_if_exists ~reader ~file_options phantom_acc path);
        lazy (path_if_exists_with_file_exts ~reader ~file_options phantom_acc path file_exts);
        lazy (resolve_package ~options ~reader ?phantom_acc path);
      ]

  let rec node_module ~options ~reader node_modules_containers file ?phantom_acc dir r =
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
                           ?phantom_acc
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
               ?phantom_acc
               (Filename.dirname dir)
               r
          );
      ]

  (* The flowconfig option `module.system.node.allow_root_relative` tells Flow
   * to resolve requires like `require('foo/bar.js')` relative to the project
   * root directory. This is something bundlers like Webpack can be configured
   * to do. *)
  let resolve_root_relative ~options ~reader ?phantom_acc import_str =
    if Options.node_resolver_allow_root_relative options then
      let dirnames = Options.node_resolver_root_relative_dirnames options in
      let root = Options.root options |> Path.to_string in
      let f dirname =
        let root =
          if dirname = "" then
            root
          else
            Filename.concat root dirname
        in
        lazy (resolve_relative ~options ~reader ?phantom_acc root import_str)
      in
      lazy_seq (Base.List.map ~f dirnames)
    else
      None

  let resolve_import ~options ~reader node_modules_containers f ?phantom_acc import_str =
    let file = File_key.to_string f in
    let dir = Filename.dirname file in
    if is_relative_or_absolute import_str then
      resolve_relative ~options ~reader ?phantom_acc dir import_str
    else
      lazy_seq
        [
          lazy (resolve_root_relative ~options ~reader ?phantom_acc import_str);
          lazy (node_module ~options ~reader node_modules_containers f ?phantom_acc dir import_str);
        ]

  let imported_module ~options ~reader node_modules_containers file ?phantom_acc r =
    let candidates = module_name_candidates ~options r in
    match
      List.find_map
        (resolve_import ~options ~reader node_modules_containers file ?phantom_acc)
        (Nel.to_list candidates)
    with
    | Some m -> Ok m
    | None ->
      (* For the Node module system, we always use the original unmapped name in
       * error messages, so we never need to store a mapped name.
       *
       * TODO: This means that name mappers can not force a mapped module name
       * to resolve to a libdef, since we try to resolve to libdef modules
       * during check in the `Error _` case. *)
      Error None

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
    | File_key.LibFile file
    | File_key.SourceFile file
    | File_key.JsonFile file
    | File_key.ResourceFile file ->
      Filename.basename file |> Filename.chop_extension

  let is_mock =
    let mock_path = Str.regexp ".*/__mocks__/.*" in
    function
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

  let is_within_node_modules options =
    let root = Options.root options in
    let options = Options.file_options options in
    Files.is_within_node_modules ~root ~options

  let exported_module options =
    let is_haste_file = is_haste_file options in
    let is_within_node_modules = is_within_node_modules options in
    fun file info ->
      match (file, info) with
      | (File_key.SourceFile _, `Module info) ->
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
      | (File_key.JsonFile path, `Package pkg) ->
        if Package_json.haste_commonjs pkg || not (is_within_node_modules path) then
          Package_json.name pkg
        else
          None
      | _ ->
        (* Lib files, resource files, etc don't have any fancy haste name *)
        None

  let package_dir_opt ~reader addr =
    if Parsing_heaps.Reader_dispatcher.is_package_file ~reader addr then
      Some (Parsing_heaps.read_file_name addr |> Filename.dirname)
    else
      None

  let resolve_haste_module ~options ~reader ?phantom_acc ~dir r =
    let (name, subpath) =
      match String.split_on_char '/' r with
      | [] -> (r, [])
      | scope :: package :: rest when String.starts_with ~prefix:"@" scope ->
        (scope ^ "/" ^ package, rest)
      | package :: rest -> (package, rest)
    in
    let mname = Modulename.String name in
    match Parsing_heaps.Reader_dispatcher.get_provider ~reader mname with
    | Some addr ->
      (match (package_dir_opt ~reader addr, subpath) with
      | (Some package_dir, []) -> Node.resolve_package ~options ~reader ?phantom_acc package_dir
      | (Some package_dir, subpath) ->
        (* add a phantom dep on the package name, so we re-resolve the subpath
           if the package gets a new provider *)
        record_phantom_dependency mname phantom_acc;

        let path = Files.construct_path package_dir subpath in
        Node.resolve_relative ~options ~reader ?phantom_acc dir path
      | (None, []) -> Some mname
      | (None, _ :: _) ->
        (* if r = foo/bar and foo is a regular module, don't resolve.
           TODO: could we provide a better error than just failing to resolve?

           we do need to add a phantom dep on the module, so we re-resolve
           if the provider changes to a package. *)
        record_phantom_dependency mname phantom_acc;
        None)
    | None ->
      record_phantom_dependency mname phantom_acc;
      None

  let resolve_import ~options ~reader node_modules_containers f ?phantom_acc r =
    let file = File_key.to_string f in
    let dir = Filename.dirname file in
    if is_relative_or_absolute r then
      Node.resolve_relative ~options ~reader ?phantom_acc dir r
    else
      lazy_seq
        [
          lazy (resolve_haste_module ~options ~reader ?phantom_acc ~dir r);
          lazy (Node.resolve_root_relative ~options ~reader ?phantom_acc r);
          lazy (Node.node_module ~options ~reader node_modules_containers file ?phantom_acc dir r);
        ]

  let imported_module ~options ~reader node_modules_containers file ?phantom_acc r =
    (* For historical reasons, the Haste module system always picks the first
     * matching candidate, unlike the Node module system which picks the first
     * "valid" matching candidate. *)
    let candidates = module_name_candidates ~options r in
    let r = Nel.hd candidates in
    match resolve_import ~options ~reader node_modules_containers file ?phantom_acc r with
    | exception exn ->
      let exn = Exception.wrap exn in
      raise (Failed_to_resolve_module { name = r; file; exn })
    | Some m -> Ok m
    | None ->
      (* If the candidates list is a singleton, then no name mappers applied,
       * and we failed to resolve the unmapped name. Otherwise, `r` is the
       * chosen mapped name and we store it for error reporting. *)
      let mapped_name =
        match Nel.tl candidates with
        | [] -> None
        | _ -> Some r
      in
      Error mapped_name

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

let imported_module ~options ~reader ~node_modules_containers file ?phantom_acc r =
  let module M = (val get_module_system options) in
  M.imported_module ~options ~reader node_modules_containers file ?phantom_acc r

let choose_provider ~options m files errmap =
  let module M = (val get_module_system options) in
  M.choose_provider m files errmap

(******************)
(***** public *****)
(******************)

let add_parsed_resolved_requires ~mutator ~reader ~options ~node_modules_containers file =
  let file_addr = Parsing_heaps.get_file_addr_unsafe file in
  let parse = Parsing_heaps.Mutator_reader.get_typed_parse_unsafe ~reader file file_addr in
  let requires = Parsing_heaps.read_requires parse in
  let phantom_acc = ref Modulename.Set.empty in
  let resolved_modules =
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    Array.map
      (fun mref -> imported_module file mref ~options ~reader ~node_modules_containers ~phantom_acc)
      requires
  in
  Parsing_heaps.Resolved_requires_mutator.add_resolved_requires
    mutator
    file_addr
    parse
    (resolved_modules, !phantom_acc)

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
let commit_modules ~workers ~options dirty_modules =
  let module Heap = SharedMem.NewAPI in
  let debug = Options.is_debug_mode options in
  let commit_haste (unchanged, errmap) mname name =
    let m = Parsing_heaps.get_haste_module_unsafe name in
    let provider_ent = Heap.get_haste_provider m in
    let all_providers =
      let f acc f =
        let key = Parsing_heaps.read_file_key f in
        FilenameMap.add key f acc
      in
      Heap.get_haste_all_providers_exclusive m
      |> List.fold_left f FilenameMap.empty
      |> FilenameMap.bindings
    in
    let old_provider = Heap.entity_read_latest provider_ent in
    let (new_provider, errmap) = choose_provider ~options name all_providers errmap in
    match (old_provider, new_provider) with
    | (_, None) ->
      (* TODO: Clean up modules which have no providers and no dependents. At
       * this point we don't have enough information to remove a module because
       * we might gain new dependents in the resolve require step after commit
       * modules.
       *
       * X-ref update revdeps in parsing heaps, where a module can lose its last
       * dependent. *)
      if debug then prerr_endlinef "no remaining providers: %s" name;
      Heap.entity_advance provider_ent None;
      (unchanged, errmap)
    | (None, Some p) ->
      (* When can this happen? Either m pointed to a file that used to
         provide m and changed or got deleted (causing m to be in
         old_modules), or m didn't have a provider before. *)
      if debug then prerr_endlinef "initial provider %s -> %s" name (Parsing_heaps.read_file_name p);
      Heap.entity_advance provider_ent (Some p);
      (unchanged, errmap)
    | (Some old_p, Some new_p) ->
      if Heap.files_equal old_p new_p then (
        (* When can this happen? Say m pointed to f before, a different file
           f' that provides m changed (so m is not in old_modules), but f
           continues to be the chosen provider = p (winning over f'). *)
        if debug then
          prerr_endlinef "unchanged provider: %s -> %s" name (Parsing_heaps.read_file_name new_p);
        let unchanged =
          (* Even if the module has the same provider file, we might need to
           * treat this module as changed. Remember that we use changed modules
           * to get the set of dirty direct dependents -- dependents which can
           * not be found through the server env's dependency graph.
           *
           * Specifically, we care about parsed<->unparsed transitions.
           * 1. Providers which were unparsed, now parsed, which had dependents
           * 2. Providers which were parsed, now unparsed, which have dependents
           *
           * These dependents are not included in the server env dep graph. *)
          let parse_ent = Heap.get_parse new_p in
          let old_typed = Option.bind (Heap.entity_read_committed parse_ent) Heap.coerce_typed in
          let new_typed = Option.bind (Heap.entity_read_latest parse_ent) Heap.coerce_typed in
          match (old_typed, new_typed) with
          | (Some _, None)
          | (None, Some _) ->
            unchanged
          | _ -> Modulename.Set.add mname unchanged
        in
        (unchanged, errmap)
      ) else (
        (* When can this happen? Say m pointed to f before, a different file
           f' that provides m changed (so m is not in old_modules), and
           now f' becomes the chosen provider = p (winning over f). *)
        if debug then
          prerr_endlinef
            "new provider: %s -> %s replaces %s"
            name
            (Parsing_heaps.read_file_name new_p)
            (Parsing_heaps.read_file_name old_p);
        Heap.entity_advance provider_ent (Some new_p);
        (unchanged, errmap)
      )
  in
  let commit_file unchanged mname file_key =
    let get_parses key =
      match Parsing_heaps.get_file_addr key with
      | None -> (None, None)
      | Some file ->
        let ent = Heap.get_parse file in
        (Heap.entity_read_committed ent, Heap.entity_read_latest ent)
    in
    let decl_key = File_key.with_suffix file_key Files.flow_ext in
    let (old_decl_parse, new_decl_parse) = get_parses decl_key in
    let (old_impl_parse, new_impl_parse) = get_parses file_key in
    match (old_decl_parse, new_decl_parse, old_impl_parse, new_impl_parse) with
    | (None, None, None, None) -> Modulename.Set.add mname unchanged
    | (None, Some _, _, _)
    | (Some _, None, _, _)
    | (None, None, Some _, None)
    | (None, None, None, Some _) ->
      (* If the provider file was created or deleted, then we need to track any
       * dependents, because phantom edges are not in the server env dependency
       * graph, but any phantom dependents need to be rechecked. *)
      unchanged
    | (Some old_parse, Some new_parse, _, _)
    | (None, None, Some old_parse, Some new_parse) ->
      (* If the provider file did a parse<->unparsed transition, we need to
       * track any dependents, because edges involving unparsed files are not in
       * the server env dependency graph, but any dependents need to be
       * rechecked. *)
      (match (Heap.coerce_typed old_parse, Heap.coerce_typed new_parse) with
      | (Some _, None)
      | (None, Some _) ->
        unchanged
      | _ -> Modulename.Set.add mname unchanged)
  in
  let f ((unchanged, errmap) as acc) mname =
    match mname with
    | Modulename.String name -> commit_haste acc mname name
    | Modulename.Filename key -> (commit_file unchanged mname key, errmap)
  in
  let%lwt (unchanged, duplicate_providers) =
    MultiWorkerLwt.call
      workers
      ~job:(List.fold_left f)
      ~neutral:(Modulename.Set.empty, SMap.empty)
      ~merge:(fun (a1, a2) (b1, b2) -> (Modulename.Set.union a1 b1, SMap.union a2 b2))
      ~next:(MultiWorkerLwt.next workers (Modulename.Set.elements dirty_modules))
  in
  let changed_modules = Modulename.Set.diff dirty_modules unchanged in
  if debug then prerr_endlinef "*** done committing modules ***";
  Lwt.return (changed_modules, duplicate_providers)
