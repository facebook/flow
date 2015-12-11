(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module is the entry point of the typechecker. It sets up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

open Utils
open Utils_js
open Sys_utils

module Ast = Spider_monkey_ast
module Flow = Flow_js
module Modes = Modes_js
module Reason = Reason_js
module ErrorSet = Errors_js.ErrorSet

module NameSet = Set.Make(Modulename)

(* Subset of a file's context, with the important distinction that module
   references in the file have been resolved to module names. *)
type info = {
  file: filename; (* file name *)
  _module: Modulename.t; (* module name *)
  required: NameSet.t; (* required module names *)
  require_loc: Loc.t SMap.t; (* statement locations *)
  resolved_modules: Modulename.t SMap.t; (* map from module references in file
                                            to module names they resolve to *)
  phantom_dependents: SSet.t; (* set of paths that were looked up but not found
                                 when resolving module references in the file:
                                 when the paths come into existence, the module
                                 references need to be re-resolved. *)
  checked: bool; (* in flow? *)
  parsed: bool; (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

let flow_options : Options.options option ref = ref None (*Options.default_options*)

let get_flow_options () =
  match !flow_options with
  | Some opts -> opts
  | None ->
     assert_false ("Attempted to use flow_options before " ^
                  "Modules_js was initialized.")

let get_config_options () =
  let root = (get_flow_options ()).Options.opt_root in
  let config = FlowConfig.get root in
  FlowConfig.(config.options)

let replace_name_mapper_template_tokens =
  let project_root_token = Str.regexp_string "<PROJECT_ROOT>" in

  (* Escape things like \1 and \2 which are interpreted as special references
   * in a template. So \1 will become \\1 *)
  let escape_template =
    Str.global_replace (Str.regexp "\\\\[0-9]+") "\\\\\\0" in

  fun opts template ->
    let root_path = escape_template (Path.to_string opts.Options.opt_root) in
    Str.global_replace project_root_token root_path template


let choose_provider_and_warn_about_duplicates =
  let is_flow_ext file = Loc.check_suffix file FlowConfig.flow_ext in

  let warn_duplicate_providers m current modules errmap =
    List.fold_left (fun acc f ->
      let w = Flow.new_warning [
        Reason.mk_reason m Loc.({ none with source = Some f }),
          "Duplicate module provider";
        Reason.mk_reason "current provider"
          Loc.({ none with source = Some current }),
          ""] in
      FilenameMap.add f (ErrorSet.singleton w) acc
    ) errmap modules in

    fun m errmap providers fallback ->
      let definitions, implementations =
        List.partition is_flow_ext providers in
      match implementations, definitions with
      (* If there are no definitions or implementations, use the fallback *)
      | [], [] -> fallback (), errmap
      (* Else if there are no definitions, use the first implementation *)
      | impl::dup_impls, [] ->
          impl, warn_duplicate_providers m impl dup_impls errmap
      (* Else use the first definition *)
      | [], defn::dup_defns ->
          defn, warn_duplicate_providers m defn dup_defns errmap
      (* Don't complain about the first implementation being a duplicate *)
      | impl::dup_impls, defn::dup_defns ->
          let errmap = errmap
            |> warn_duplicate_providers m impl dup_impls
            |> warn_duplicate_providers m defn dup_defns in
          defn, errmap

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
let module_name_candidates name =
  if Hashtbl.mem module_name_candidates_cache name then (
    Hashtbl.find module_name_candidates_cache name
  ) else (
    let flow_options = get_flow_options () in
    let mappers = flow_options.Options.opt_module_name_mappers in
    let map_name mapped_names (regexp, template) =
      let template = replace_name_mapper_template_tokens flow_options template in
      let new_name = Str.global_replace regexp template name in
      if new_name = name then mapped_names else new_name::mapped_names
    in
    let mapped_names = List.rev (name::(List.fold_left map_name [] mappers)) in
    Hashtbl.add module_name_candidates_cache name mapped_names;
    mapped_names
  )

(****************** shared dependency map *********************)

(* map from module name to filename *)
module NameHeap = SharedMem.WithCache (Modulename) (struct
  type t = filename
  let prefix = Prefix.make()
end)

(* map from filename to module info *)
(* note: currently we may have many files for one module name.
   this is an issue. *)
module InfoHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
  type t = info
  let prefix = Prefix.make()
end)

(* Given a set of newly added files, decide whether the resolution of module
   references in f "depended" (see above) on any of those files, and if so, add
   f to acc. *)
let resolution_path_dependency files acc f =
  match InfoHeap.get f with
  | Some { phantom_dependents = fs; _ } when fs |> SSet.exists (fun f ->
      FilenameSet.mem (Loc.SourceFile f) files
    ) -> FilenameSet.add f acc
  | _ -> acc

(** module systems **)

(* shared heap for package.json tokens by filename *)
module PackageHeap = SharedMem.WithCache (String) (struct
    type t = Ast.Expression.t SMap.t
    let prefix = Prefix.make()
  end)

(* shared heap for package.json directories by package name *)
module ReversePackageHeap = SharedMem.WithCache (String) (struct
    type t = string
    let prefix = Prefix.make()
  end)

let get_key key tokens = Ast.(
  match SMap.get (spf "\"%s\"" key) tokens with
  | Some (_, Expression.Literal { Literal.value = Literal.String name; _ }) ->
      Some name
  | _ -> None
)

let add_package package =
  let json = cat package in
  let tokens, errors = FlowJSON.parse_object json (Loc.SourceFile package) in
  PackageHeap.add package tokens;
  (match get_key "name" tokens with
  | Some name ->
    ReversePackageHeap.add name (Filename.dirname package)
  | None -> ());
  if List.length errors = 0 then None else
    let fold_error e_set error =
      Errors_js.(ErrorSet.add (parse_error_to_flow_error error) e_set)
    in
    Some(List.fold_left fold_error Errors_js.ErrorSet.empty errors)

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and docblock info, make the name of the module it exports. *)
  val exported_module: filename -> Docblock.t -> Modulename.t

  (* Given a file and a reference in it to an imported module, make the name of
     the module it refers to. If given an optional reference to an accumulator,
     record paths that were looked up but not found during resolution. *)
  val imported_module: ?path_acc:SSet.t ref -> filename -> string -> Modulename.t

  (* for a given module name, choose a provider from among a set of
    files with that exported name. also check for duplicates and
    generate warnings, as dictated by module system rules. *)
  val choose_provider:
    string ->   (* module name *)
    FilenameSet.t ->   (* set of candidate provider files *)
    (* map from files to error sets (accumulator) *)
    Errors_js.ErrorSet.t FilenameMap.t ->
    (* file, error map (accumulator) *)
    (filename * Errors_js.ErrorSet.t FilenameMap.t)

end

(****************** Node module system *********************)

let seq f g =
  match f () with
  | Some x -> Some x
  | None -> g ()

let rec seqf f = function
  | x :: xs ->
    (match f x with
    | Some v -> Some v
    | None -> seqf f xs)
  | [] -> None

(*********************************)

(* TODO this exists only until we start resolving files using
   NameHeap. unfortunately that will require more refactoring
   than it should, since imported_module is currently called
   during local inference, and simply storing raw module names
   in cx.required et al and looking them up at merge time appears
   to violate some well-hidden private agreements. TODO *)

(* only purpose here is to guarantee a case-sensitive file exists
   and try to keep it from being too horrendously expensive *)

let case_sensitive =
  not (Sys.file_exists (String.uppercase (Sys.getcwd ())))

(* map of dirs to file lists *)
let files_in_dir = ref SMap.empty

(* called from Types_js.typecheck, so we rebuild every time *)
let clear_filename_cache () =
  files_in_dir := SMap.empty

(* case-sensitive dir_exists  *)
let rec dir_exists dir =
  try Sys.is_directory dir && (case_sensitive || file_exists dir)
  with _ -> false

(* when system is case-insensitive, do our own file exists check *)
and file_exists path =
  (* case doesn't matter for "/", ".", "..." and these serve as a base-case for
   * case-insensitive filesystems *)
  let dir = Filename.dirname path in
  if (
    case_sensitive
    || path = Filename.current_dir_name
    || path = Filename.parent_dir_name
    || path = dir
  ) then Sys.file_exists path
  else (
    let files = match SMap.get dir !files_in_dir with
    | Some files -> files
    | None ->
        let files =
          if dir_exists dir
          then set_of_list (Array.to_list (Sys.readdir dir))
          else SSet.empty in
        files_in_dir := SMap.add dir files !files_in_dir;
        files
    in SSet.mem (Filename.basename path) files
  )

let resolve_symlinks path =
  Path.to_string (Path.make path)

(*******************************)

module Node = struct
  let exported_module file info =
    if Loc.check_suffix file FlowConfig.flow_ext
    then Modulename.Filename (Loc.chop_suffix file FlowConfig.flow_ext)
    else Modulename.Filename file

  let record_path path = function
    | None -> ()
    | Some paths -> paths := SSet.add path !paths

  let path_if_exists path_acc =
    let path_exists no_dir path =
      (file_exists path) &&
        not FlowConfig.(is_excluded (get_unsafe ()) path) &&
        not (no_dir && dir_exists path)
    in fun ?(no_dir=true) path ->
      let path = resolve_symlinks path in
      let declaration_path = path ^ FlowConfig.flow_ext in
      if path_exists no_dir declaration_path ||
        path_exists no_dir path
      then Some path
      else (record_path path path_acc; None)

  let parse_main path_acc package =
    let package = resolve_symlinks package in
    if not (file_exists package) ||
      FlowConfig.(is_excluded (get_unsafe ()) package)
    then None
    else
      let tokens = match PackageHeap.get package with
      | Some tokens -> tokens
      | None -> failwith (spf
          "internal error: package %s not found in PackageHeap" package)
      in
      let dir = Filename.dirname package in
      match get_key "main" tokens with
      | None -> None
      | Some file ->
        let opts = get_config_options () in
        let path = Files_js.normalize_path dir file in
        seq
          (fun () -> path_if_exists path_acc ~no_dir:true path)
          (fun () -> seq
            (fun () ->
              seqf
                (fun ext -> path_if_exists path_acc (path ^ ext))
                (SSet.elements opts.FlowConfig.Opts.module_file_exts)
            )
            (fun () ->
              let path = Filename.concat path "index.js" in
              path_if_exists path_acc path
            )
          )

  let resolve_relative ?path_acc root_path rel_path =
    let path = Files_js.normalize_path root_path rel_path in
    let opts = get_config_options () in
    if Files_js.is_flow_file path
    then path_if_exists path_acc path
    else seq
      (fun () -> seqf
        (fun ext -> path_if_exists path_acc (path ^ ext))
        (SSet.elements opts.FlowConfig.Opts.module_file_exts)
      )
      (fun () -> seq
        (fun () -> parse_main path_acc (Filename.concat path "package.json"))
        (fun () -> path_if_exists path_acc (Filename.concat path "index.js"))
      )

  let rec node_module path_acc dir r =
    let opts = get_config_options () in
    seq
      (fun () -> seqf
        (resolve_relative ?path_acc dir)
        (opts.FlowConfig.Opts.node_resolver_dirnames |> List.map (fun dirname ->
          spf "%s%s%s" dirname Filename.dir_sep r
        ))
      )
      (fun () ->
        let parent_dir = Filename.dirname dir in
        if dir = parent_dir then None
        else node_module path_acc (Filename.dirname dir) r
      )

  let absolute r =
    Str.string_match Files_js.absolute_path r 0

  let explicitly_relative r =
    Str.string_match Files_js.current_dir_name r 0
    || Str.string_match Files_js.parent_dir_name r 0

  let resolve_import ?path_acc file import_str =
    let dir = Filename.dirname file in
    if explicitly_relative import_str || absolute import_str
    then resolve_relative ?path_acc dir import_str
    else node_module path_acc dir import_str

  let imported_module ?path_acc file import_str =
    let file = string_of_filename file in
    let candidates = module_name_candidates import_str in

    let rec choose_candidate = function
      | [] -> None
      | candidate :: candidates ->
        match resolve_import ?path_acc file candidate with
        | None -> choose_candidate candidates
        | Some _ as result -> result
    in
    match choose_candidate candidates with
    | Some(str) -> Modulename.Filename (Loc.SourceFile str)
    | None -> Modulename.String import_str

  (* in node, file names are module names, as guaranteed by
     our implementation of exported_name, so anything but a
     singleton provider set is craziness. *)
  let choose_provider m files errmap =
    let files = FilenameSet.elements files in
    let fallback () =
      failwith (spf "internal error: empty provider set for module %S" m) in
    choose_provider_and_warn_about_duplicates m errmap files fallback

end

(****************** Haste module system *********************)

module Haste: MODULE_SYSTEM = struct
  let module_name_of file =
    try Filename.chop_extension file
    (* TODO: Think of what we should do when we got a filename without
     an extension *)
    with _ -> file

  let short_module_name_of = function
    | Loc.Builtins -> assert false
    | Loc.LibFile file | Loc.SourceFile file ->
        Filename.basename file |> Filename.chop_extension

  let is_mock =
    let mock_path = Str.regexp ".*/__mocks__/.*" in
    function
    | Loc.Builtins -> false
    | Loc.LibFile file | Loc.SourceFile file ->
        Str.string_match mock_path file 0

  let rec exported_module file info =
    if is_mock file
    then Modulename.String (short_module_name_of file)
    else match Docblock.providesModule info with
      | Some m -> Modulename.String m
      | None ->
          (* If foo.js.flow doesn't have a @providesModule, then look at foo.js
           * and use its @providesModule instead *)
          if Loc.check_suffix file FlowConfig.flow_ext
          then
            let file_without_flow_ext = Loc.chop_suffix file FlowConfig.flow_ext in
            if Parsing_service_js.has_ast file_without_flow_ext
            then
              let _, info =
                Parsing_service_js.get_ast_and_info_unsafe file_without_flow_ext in
              exported_module file_without_flow_ext info
            else
              Modulename.Filename (file_without_flow_ext)
          else
            Modulename.Filename file

  let expanded_name r =
    match Str.split_delim (Str.regexp_string "/") r with
    | [] -> None
    | package_name::rest ->
        ReversePackageHeap.get package_name |> opt_map (fun package ->
          Files_js.construct_path package rest
        )

  (* similar to Node resolution, with possible special cases *)
  let resolve_import ?path_acc file r =
    let file = string_of_filename file in
    seq
      (fun () -> Node.resolve_import ?path_acc file r)
      (fun () ->
        match expanded_name r with
        | Some r -> Node.resolve_relative ?path_acc (Filename.dirname file) r
        | None -> None
      )

  let imported_module ?path_acc file imported_name =
    let candidates = module_name_candidates imported_name in

    (**
     * In Haste, we don't have an autoritative list of all valid module names
     * until after all modules have been sweeped (because the module name is
     * specified in the contents of the file). So, unlike the node module
     * system, we can't run through the list of mapped module names and only
     * choose the first one that is valid.
     *
     * Therefore, for the Haste module system, we simply always pick the first
     * matching candidate (rather than the first *valid* matching candidate).
     *)
    let chosen_candidate = List.hd candidates in

    match resolve_import ?path_acc file chosen_candidate with
    | Some(name) -> Modulename.Filename (Loc.SourceFile name)
    | None -> Modulename.String chosen_candidate

  (* in haste, many files may provide the same module. here we're also
     supporting the notion of mock modules - allowed duplicates used as
     fallbacks. we prefer the non-mock if it exists, otherwise choose an
     arbitrary mock, if any exist. if multiple non-mock providers exist,
     we pick one arbitrarily and issue duplicate module warnings for the
     rest. *)
  let choose_provider m files errmap =
    match FilenameSet.elements files with
    | [] ->
        failwith (spf "internal error: empty provider set for module %S" m)
    | [f] ->
        f, errmap
    | files ->
        let mocks, non_mocks = List.partition is_mock files in
        let fallback () = List.hd mocks in
        choose_provider_and_warn_about_duplicates m errmap non_mocks fallback

end

(****************** module system switch *********************)

(* Switch between module systems, based on environment. We could eventually use
   functors, but that seems like overkill at this point. *)

let module_system_table =
  let table = Hashtbl.create 2 in
  Hashtbl.add table "node" (module Node: MODULE_SYSTEM);
  Hashtbl.add table "haste" (module Haste: MODULE_SYSTEM);
  table

let module_system = ref (module Node: MODULE_SYSTEM)

let init opts =
  flow_options := Some(opts);
  module_system := Hashtbl.find module_system_table opts.Options.opt_module

let exported_module file info =
  let module M = (val !module_system) in
  M.exported_module file info

let imported_module ?path_acc file r =
  let module M = (val !module_system) in
  M.imported_module ?path_acc file r

let imported_modules file reqs =
  (* Resolve all reqs relative to file. Accumulate dependent paths in
     path_acc. Return the map of reqs to their resolved names, and the set
     containing the resolved names. *)
  let path_acc = ref SSet.empty in
  let set, map = SSet.fold (fun r (set, map) ->
    let resolved_r = imported_module ~path_acc file r in
    NameSet.add resolved_r set, SMap.add r resolved_r map
  ) reqs (NameSet.empty, SMap.empty) in
  set, map, !path_acc

(* Look up cached resolved module. *)
let cached_resolved_module file r =
  match InfoHeap.get file with
  | Some { resolved_modules; _ } -> SMap.get r resolved_modules
  | None -> None

(* Optimized module resolution function that goes through cache. *)
let find_resolved_module file r =
  match cached_resolved_module file r with
  | Some resolved_r -> resolved_r
  | None -> imported_module file r

let choose_provider m files errmap =
  let module M = (val !module_system) in
  M.choose_provider m files errmap

(****************** reverse import utils *********************)

(* map from module name to modules which import it. *)
(* note: this is outside InfoHeap because InfoHeap gets changed concurrently
   by the infer jobs, we cannot simply build the information up there *)
let reverse_imports_map = Hashtbl.create 0

let reverse_imports_clear module_name =
  Hashtbl.remove reverse_imports_map module_name

let reverse_imports_get module_name =
  try Some (Hashtbl.find reverse_imports_map module_name)
  with Not_found -> None

let reverse_imports_track importer_name requires =
  let add_requires module_name =
    let reverse_imports = match reverse_imports_get module_name with
    | Some reverse_imports ->
        reverse_imports
    | None ->
        NameSet.empty
    in
    let new_reverse_imports =
      NameSet.add importer_name reverse_imports in
    Hashtbl.replace reverse_imports_map module_name new_reverse_imports
  in
  NameSet.iter add_requires requires

let reverse_imports_untrack importer_name requires =
  let remove_requires module_name =
    match reverse_imports_get module_name with
    | Some reverse_imports ->
        let new_reverse_imports =
          NameSet.remove importer_name reverse_imports in
        if not (NameSet.is_empty new_reverse_imports)
        then Hashtbl.replace reverse_imports_map module_name new_reverse_imports
        else begin
          (* in case the reverse import map is empty we might have hit a case
             where the module we keep track of exists but is not imported by
             anything anymore. in this case we do not clear the tracking
             information but simply set it to empty. this makes sure that
             the information returned by get-imported-by is never confusing
             for exisitng, but not imported modules *)
          if NameHeap.mem module_name
          then Hashtbl.replace reverse_imports_map module_name NameSet.empty
          else reverse_imports_clear module_name
        end
    | None ->
        ()
  in NameSet.iter remove_requires requires

(******************)
(***** public *****)
(******************)

let module_exists = NameHeap.mem

let get_file m =
  match NameHeap.get m with
  | Some file -> file
  | None -> failwith
      (spf "file name not found for module %s" (Modulename.to_string m))

(* Gets the filename for a module without failing when it does not exist *)
let get_module_file m =
  NameHeap.get m

let get_module_info f =
  match InfoHeap.get f with
  | Some info -> info
  | None -> failwith
      (spf "module info not found for file %s" (string_of_filename f))

let get_module_name f =
  (get_module_info f)._module

let add_reverse_imports filenames =
  List.iter (fun filename ->
    let { _module = name; required = req; _ } = get_module_info filename in
    (* we only add requriements from actual module providers. this avoids
       strange states when two files provide the same module. *)
    match get_module_file name with
    | Some file when file = filename ->
        (* we need to make sure we are in the reverse import heap to avoid
           confusing behavior when querying it *)
        (if not (Hashtbl.mem reverse_imports_map name)
        then Hashtbl.add reverse_imports_map name NameSet.empty);
        reverse_imports_track name req
    | _ -> ()
  ) filenames

let get_reverse_imports module_name =
  match module_name, reverse_imports_get module_name with
  | Modulename.Filename filename, None ->
      let { _module = name; _ } = get_module_info filename in
      reverse_imports_get name
  | _, result -> result

(* Extract and process information from context. In particular, resolve
   references to required modules in a file, and record the results.  *)
let info_of cx =
  let file = Context.file cx in
  let required, resolved_modules, phantom_dependents =
    imported_modules file (Context.required cx) in
  let require_loc = SMap.fold
    (fun r loc require_loc ->
      let resolved_r = SMap.find_unsafe r resolved_modules in
      SMap.add (Modulename.to_string resolved_r) loc require_loc)
    (Context.require_loc cx) SMap.empty in
  {
    file;
    _module = Context.module_name cx;
    required;
    require_loc;
    resolved_modules;
    phantom_dependents;
    checked = Context.is_checked cx;
    parsed = true;
  }

(* during inference, we add per-file module info to the shared heap
   from worker processes.
   Note that we wait to choose providers until inference is complete. *)
let add_module_info cx =
  let info = info_of cx in
  InfoHeap.add info.file info

(* We need to track files that have failed to parse. This begins with
   adding tracking records for unparsed files to InfoHeap. They never
   become providers - the process of committing modules happens after
   parsed files are finished with local inference. But since we guess
   the module names of unparsed files, we're able to tell whether an
   unparsed file has been required/imported.
 *)
let add_unparsed_info ~force_check file =
  let filename = Loc.(match file with
  | LibFile filename | SourceFile filename -> filename
  | Builtins -> assert false
  ) in
  let content = cat filename in
  let docblock = Docblock.extract filename content in
  let _module = exported_module file docblock in
  let checked =
    force_check ||
    Loc.source_is_lib_file file ||
    Docblock.is_flow docblock ||
    Docblock.isDeclarationFile docblock
  in
  let info = { file; _module; checked; parsed = false;
    required = NameSet.empty;
    require_loc = SMap.empty;
    resolved_modules = SMap.empty;
    phantom_dependents = SSet.empty;
  } in
  InfoHeap.add file info

(* Note that the module provided by a file is always accessible via its full
   path, so that it may be imported by specifying (a part of) that path in any
   module system. So, e.g., a file whose full path is /foo/bar.js is considered
   to export a module by that name, so that it is always possible to import it
   from any other file in the file system by using a relative path or some other
   file system navigation convention. The Node module system relies on this
   basic setup.

   In addition, a file may or may not export its module by another name: this
   name is typically shorter, but can be used unambiguously throughout the file
   system to import the module, and this access mechanism is somewhat robust to
   moving files around in the file system. Thus, /foo/bar.js may also export its
   module by the name Bar, using a custom module system like Haste, either
   explicitly (by mentioning Bar in the file) or implicitly (following some
   convention), so that it can be imported from any other file in the file
   system by the name Bar. Other combinations are possible: e.g., all files in a
   directory may export their modules via paths relative to a package name, and
   files elsewhere in the file system can import those modules by providing
   paths relative to that package name. So, e.g., /foo/bar.js may export its
   module via the name Foo/bar.js, with the name Foo may be specified in a
   config file under /foo, and other files may still be able to import it by
   that name when the /foo directory is moved to, say, /qux/foo. *)

(* hash table from module names to all known provider files.
   maintained and used by commit_modules and remove_files *)
let all_providers = Hashtbl.create 0

let add_provider f m =
  let provs = try FilenameSet.add f (Hashtbl.find all_providers m)
    with Not_found -> FilenameSet.singleton f in
  Hashtbl.replace all_providers m provs

let remove_provider f m =
  let provs = try FilenameSet.remove f (Hashtbl.find all_providers m)
    with Not_found -> failwith (spf
      "can't remove provider %s of %S, not found in all_providers"
      (string_of_filename f) (Modulename.to_string m))
  in
  Hashtbl.replace all_providers m provs

let get_providers = Hashtbl.find all_providers

(* inferred is a list of inferred file names.
   removed is a set of removed module names.
   modules provided by inferred files may or may not have a provider.
   modules named in removed definitely do not have a provider.

   Preconditions:
   1. all files in inferred have entries in InfoHeap (true if
   infer is properly calling add_module_info for every file
   successfully inferred)
   2. all modules not mentioned in removed, but provided by one or more
   files in InfoHeap, have some provider registered in NameHeap.
   (However, the current provider may not be the one we now want,
   given newly inferred files.)
   3. conversely all modules in removed lack a provider in NameHeap.

   Postconditions:
   1. all modules provided by at least 1 file
   in InfoHeap have a provider registered in NameHeap, and it's the
   provider we want according to our precedence and scoping rules.

   We make use of a shadow map in the master process which maintains
   a view of what's going on in NameHeap and InfoHeap, mapping module
   names to sets of filenames of providers.

   Algorithm here:
   1. add all removed modules to the set of modules to repick a provider for.
   2. add the modules provided by all inferred files to the repick set.
   3. for each module in the repick set, pick a winner from its available
   providers. if it's different than the current provider, or if there is no
   current provider, add the new provider to the list to be registered.
   4. remove the unregistered modules from NameHeap
   5. register the new providers in NameHeap
 *)
let commit_modules ?(debug=false) inferred removed =
  if debug then prerr_endlinef
    "*** committing modules inferred %d removed %d ***"
    (List.length inferred) (NameSet.cardinal removed);
  (* all removed modules must be repicked *)
  (* all modules provided by newly inferred files must be repicked *)
  let repick = List.fold_left (fun acc f ->
    let { _module = m; _ } = get_module_info f in
    let f_module = Modulename.Filename f in
    add_provider f m; add_provider f f_module;
    acc |> NameSet.add m |> NameSet.add f_module
  ) removed inferred in
  (* prep for registering new mappings in NameHeap *)
  let remove, replace, errmap = NameSet.fold (fun m (rem, rep, errmap) ->
    match get_providers m with
    | ps when FilenameSet.cardinal ps = 0 ->
        if debug then prerr_endlinef
          "no remaining providers: %S"
          (Modulename.to_string m);
        (NameSet.add m rem), rep, errmap
    | ps ->
      (* incremental: clear error sets of provider candidates *)
      let errmap = FilenameSet.fold (fun f acc ->
        FilenameMap.add f ErrorSet.empty acc) ps errmap in
      (* now choose provider for m *)
      let p, errmap = choose_provider (Modulename.to_string m) ps errmap in
      match NameHeap.get m with
      | Some f when f = p ->
          if debug then prerr_endlinef
            "unchanged provider: %S -> %s"
            (Modulename.to_string m)
            (string_of_filename p);
          rem, rep, errmap
      | Some f ->
          if debug then prerr_endlinef
            "new provider: %S -> %s replaces %s"
            (Modulename.to_string m)
            (string_of_filename p)
            (string_of_filename f);
          (NameSet.add m rem), ((m, p) :: rep), errmap
      | None ->
          if debug then prerr_endlinef
            "initial provider %S -> %s"
            (Modulename.to_string m)
            (string_of_filename p);
          (NameSet.add m rem), ((m, p) :: rep), errmap
  ) repick (NameSet.empty, [], FilenameMap.empty) in
  (* update NameHeap *)
  NameHeap.remove_batch remove;
  SharedMem.collect `gentle;
  List.iter (fun (m, p) ->
    NameHeap.add m p;
    NameHeap.add (Modulename.Filename p) p
  ) replace;
  (* now that providers are updated, update reverse dependency info *)
  add_reverse_imports inferred;
  if debug then prerr_endlinef "*** done committing modules ***";
  errmap

let clear_infos files =
  InfoHeap.remove_batch files

(* remove module mappings for given files, if they exist. Possibilities:
   1. file is current registered module provider for a given module name
   2. file is not current provider, but info is still registered
   3. file isn't in the map at all. This is an error.
   We return the set of module names whose current providers have been
   removed (#1). This is the set commit_module expects as its second
   argument.
*)
let remove_files files =
  (* files may or may not be registered as module providers.
     when they are, we need to clear their registrations *)
  let names = FilenameSet.fold (fun file names ->
      match InfoHeap.get file with
      | Some info ->
          let { _module; required; parsed; _ } = info in
          (if parsed then remove_provider file _module);
          (match NameHeap.get _module with
          | Some f when f = file -> (
              (* untrack all imports from this file. we only do this for
                 module providers to avoid inconsistencies when there are
                 multiple providers. *)
              reverse_imports_untrack _module required;
              (* when we delete a module and it was never referenced we need
                 to delete it from the reverse import heap. Otherwise, it can
                 still be looked up by using get-imported-by and that is
                 inconsistent. However, we only do so when we are the actual
                 module provider. This makes sure that exsiting modules won't
                 get erased when a duplicate is removed.
              *)
              match reverse_imports_get _module with
              | Some reverse_imports ->
                  if NameSet.is_empty reverse_imports
                  then reverse_imports_clear _module
              | None -> ()
              );
              names
              |> NameSet.add _module
              |> NameSet.add (Modulename.Filename file)
          | _ ->
              names
        )
      | None ->
          names
  ) files NameSet.empty in
  (* clear any registrations that point to infos we're about to clear *)
  (* for infos, remove_batch will ignore missing entries, no need to filter *)
  NameHeap.remove_batch names;
  clear_infos files;
  SharedMem.collect `gentle;
  (* note: only return names of modules actually removed *)
  names

(*****************************************************************************)
(* The following code used to output the dependency graph in GraphML format. *)
(*****************************************************************************)

(*
  let oc = open_out "flow.graphml" in
  output_string oc "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns/graphml\"
  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
         xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns/graphml
  http://www.yworks.com/xml/schema/graphml/1.0/ygraphml.xsd\"
         xmlns:y=\"http://www.yworks.com/xml/graphml\">
  <key id=\"D0\" for=\"node\" yfiles.type=\"nodegraphics\"/>
  <graph id=\"G0\" edgedefault=\"directed\">
";
  !dependencies |> SMap.iter (fun m (_,requires) ->
    output_string oc ("
<node id=\"" ^ m ^ "\">
      <data key=\"D0\">
        <y:ShapeNode>
          <y:NodeLabel>" ^ m ^ "</y:NodeLabel>
        </y:ShapeNode>
      </data>
    </node>
");
    requires |> SSet.iter (fun r ->
      if (SMap.mem r !dependencies) then
        output_string oc ("
<edge source=\"" ^ m ^ "\" target=\"" ^ r ^ "\"/>
")
      else ()
    )
  );
  output_string oc "
  </graph>
</graphml>
";
  close_out oc

*)
