(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

open Utils_js

module Ast = Spider_monkey_ast
module FlowError = Flow_error

module NameSet = Set.Make(Modulename)
module NameMap = MyMap.Make(Modulename)

(* Subset of a file's context, with the important distinction that module
   references in the file have been resolved to module names. *)
(** TODO [perf] Make resolved_requires tighter.
    (1) required and resolved_modules have a lot of redundancy; required is the
    value set of resolved_modules.

    (2) require_loc, too? Indexed by required.

    Also, for info:
    (1) checked? We know that requires and phantom dependents for unchecked
    files are empty.

    (2) parsed? We only care about the module provided by an unparsed file, but
    that's probably guessable.
**)
type resolved_requires = {
  required: NameSet.t; (* required module names *)
  require_loc: Loc.t SMap.t; (* statement locations *)
  resolved_modules: Modulename.t SMap.t; (* map from module references in file
                                            to module names they resolve to *)
  phantom_dependents: SSet.t; (* set of paths that were looked up but not found
                                 when resolving module references in the file:
                                 when the paths come into existence, the module
                                 references need to be re-resolved. *)
}

type info = {
  _module: Modulename.t; (* module name *)
  checked: bool; (* in flow? *)
  parsed: bool; (* if false, it's a tracking record only *)
}

type mode = ModuleMode_Checked | ModuleMode_Weak | ModuleMode_Unchecked

type error =
  | ModuleDuplicateProviderError of duplicate_provider_error

and duplicate_provider_error = {
  module_name: string;
  provider: Loc.filename;
  conflict: Loc.filename;
}

let relevant_package_keys = ["name"; "main"]
let is_relevant_key key = List.mem key relevant_package_keys

let choose_provider_and_warn_about_duplicates =
  let is_flow_ext file = Loc.check_suffix file Files.flow_ext in

  let warn_duplicate_providers m current modules errmap =
    List.fold_left (fun acc f ->
      let w = ModuleDuplicateProviderError {
        module_name = m;
        provider = current;
        conflict = f;
      } in
      FilenameMap.add f (match FilenameMap.get f acc with
        | Some errset -> w::errset
        | None -> [w]
      ) acc
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
let module_name_candidates ~options name =
  if Hashtbl.mem module_name_candidates_cache name then (
    Hashtbl.find module_name_candidates_cache name
  ) else (
    let mappers = Options.module_name_mappers options in
    let root = Options.root options
      |> Path.to_string
      |> Sys_utils.normalize_filename_dir_sep in
    let map_name mapped_names (regexp, template) =
      let new_name = name
        (* First we apply the mapper *)
        |> Str.global_replace regexp template
        (* Then we replace the PROJECT_ROOT placeholder. This works like
         * Str.global_replace except it ignores things that look like
         * backreferences, like \1 *)
        |> Str.split_delim FlowConfig.project_root_token
        |> String.concat root in
      if new_name = name then mapped_names else new_name::mapped_names
    in
    let mapped_names = List.rev (name::(List.fold_left map_name [] mappers)) in
    Hashtbl.add module_name_candidates_cache name mapped_names;
    mapped_names
  )

(****************** shared dependency map *********************)

(* map from module name to filename *)
module NameHeap = SharedMem_js.WithCache (Modulename) (struct
  type t = filename
  let prefix = Prefix.make()
  let description = "Name"
end)

let get_file = Expensive.wrap NameHeap.get
let add_file = Expensive.wrap NameHeap.add

(* map from filename to resolved requires *)
module ResolvedRequiresHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
  type t = resolved_requires
  let prefix = Prefix.make()
  let description = "ResolvedRequires"
end)

let get_resolved_requires = Expensive.wrap ResolvedRequiresHeap.get
let add_resolved_requires = Expensive.wrap ResolvedRequiresHeap.add

(* map from filename to module name *)
(* note: currently we may have many files for one module name.
   this is an issue. *)
module InfoHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
  type t = info
  let prefix = Prefix.make()
  let description = "Info"
end)

let get_info = Expensive.wrap InfoHeap.get
let add_info = Expensive.wrap InfoHeap.add

(** module systems **)

(* shared heap for package.json tokens by filename *)
module PackageHeap = SharedMem_js.WithCache (StringKey) (struct
    type t = Ast.Expression.t SMap.t
    let prefix = Prefix.make()
    let description = "Package"
  end)

(* shared heap for package.json directories by package name *)
module ReversePackageHeap = SharedMem_js.WithCache (StringKey) (struct
    type t = string
    let prefix = Prefix.make()
    let description = "ReversePackage"
  end)

let get_key key tokens = Ast.(
  match SMap.get key tokens with
  | Some (_, Expression.Literal { Literal.value = Literal.String name; _ }) ->
      Some name
  | _ -> None
)

let trim_quotes str =
  let len = String.length str in
  assert (len >= 2);
  assert (String.get str 0 = '"');
  assert (String.get str (len - 1) = '"');
  String.sub str 1 (len - 2)

let tokens_equal map1 map2 =
  if SMap.cardinal map1 <> SMap.cardinal map2 then
    false
  else
    let f key _ is_equal =
      let map1_val = get_key key map1 in
      let map2_val = get_key key map2 in
      is_equal && map1_val = map2_val
    in
    SMap.fold f map1 true

let get_package_keys filename ast =
  let open Ast in
  let open Expression.Object in
  let statement = match ast with
  | (_, [statement], _) -> statement
  | _ -> assert_false (spf "Expected %s to have a single statement." filename)
  in
  let obj = match statement with
  | _, Statement.Expression { Statement.Expression.
     expression = _, Expression.Assignment { Expression.Assignment.
       operator = Expression.Assignment.Assign;
       left = _;
       right = obj;
     };
     directive = _;
   } -> obj
  | _ -> assert_false (spf "Expected %s to be an assignment" filename)
  in
  let properties = match obj with
  | (_, Expression.Object {properties}) -> properties
  | _ -> assert_false (spf "Expected %s to have an object literal" filename)
  in
  let extract_property map = function
    | Property(_, {
        Property.key = Property.Literal(_, {Literal.raw; _;});
        value = Property.Init value;
        _;
      }) ->
        let key = trim_quotes raw in
        if is_relevant_key key then
          SMap.add key value map
        else
          map
    | _ -> SMap.empty
  in
  List.fold_left extract_property SMap.empty properties

let add_package package ast =
  let tokens = get_package_keys package ast in
  PackageHeap.add package tokens;
  match get_key "name" tokens with
  | Some name ->
    ReversePackageHeap.add name (Filename.dirname package)
  | None -> ()

let package_incompatible package ast =
  let new_tokens = get_package_keys package ast in
  let old_tokens_opt = PackageHeap.get package in
  match old_tokens_opt with
  | None -> true
  | Some old_tokens ->
    let result = not (tokens_equal old_tokens new_tokens) in
    result

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and docblock info, make the name of the module it exports. *)
  val exported_module: Options.t -> filename -> Docblock.t -> Modulename.t

  (* Given a file and a reference in it to an imported module, make the name of
     the module it refers to. If given an optional reference to an accumulator,
     record paths that were looked up but not found during resolution. *)
  val imported_module:
    options: Options.t ->
    Context.t -> Loc.t ->
    ?path_acc:SSet.t ref ->
    string -> Modulename.t

  (* for a given module name, choose a provider from among a set of
    files with that exported name. also check for duplicates and
    generate warnings, as dictated by module system rules. *)
  val choose_provider:
    string ->   (* module name *)
    FilenameSet.t ->   (* set of candidate provider files *)
    (* map from files to error sets (accumulator) *)
    error list FilenameMap.t ->
    (* file, error map (accumulator) *)
    (filename * error list FilenameMap.t)

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

let case_sensitive =
  not (Sys.file_exists (String.uppercase (Sys.getcwd ())))

(* map of dirs to file lists *)
(** TODO [perf]: investigate whether this takes too much memory **)
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

(**
 * Given a list of lazy "option" expressions, evaluate each in the list
 * sequentially until one produces a `Some` (and do not evaluate any remaining).
 *)
let lazy_seq: 'a option Lazy.t list -> 'a option =
  List.fold_left (fun acc lazy_expr ->
    match acc with
    | None -> Lazy.force lazy_expr
    | Some _ -> acc
  ) None

(*******************************)

module Node = struct
  let exported_module _ file _ =
    if Loc.check_suffix file Files.flow_ext
    then Modulename.Filename (Loc.chop_suffix file Files.flow_ext)
    else Modulename.Filename file

  let record_path path = function
    | None -> ()
    | Some paths -> paths := SSet.add path !paths

  let path_if_exists =
    let path_exists ~options path =
      (file_exists path) &&
        not (Files.is_ignored options path) &&
        not (dir_exists path)
    in fun ~options path_acc path ->
      let path = resolve_symlinks path in
      let declaration_path = path ^ Files.flow_ext in
      if path_exists ~options declaration_path ||
        path_exists ~options path
      then Some path
      else (record_path path path_acc; None)

  let path_if_exists_with_file_exts ~options path_acc path file_exts =
    lazy_seq (file_exts |> List.map (fun ext ->
      lazy (path_if_exists ~options path_acc (path ^ ext))
    ))

  let parse_main ~options cx loc path_acc package file_exts =
    let package = resolve_symlinks package in
    if not (file_exists package) || (Files.is_ignored options package)
    then None
    else
      let tokens = match PackageHeap.get package with
      | Some tokens -> tokens
      | None ->
        let project_root = Options.root options in
        let msg =
          let is_included = Files.is_included options package in
          let project_root_str = Path.to_string project_root in
          let is_contained_in_root =
            Files.is_prefix project_root_str package
          in
          let package_relative_to_root =
            spf "<<PROJECT_ROOT>>%s%s"
              (Filename.dir_sep)
              (Files.relative_path project_root_str package)
          in
          if is_included || is_contained_in_root then (
            FlowError.(EInternal (loc, PackageHeapNotFound package_relative_to_root))
          ) else (
            FlowError.EModuleOutsideRoot (loc, package_relative_to_root)
          )
        in
        Flow_js.add_output cx msg;
        SMap.empty
      in
      let dir = Filename.dirname package in
      match get_key "main" tokens with
      | None -> None
      | Some file ->
        let path = Files.normalize_path dir file in
        let path_w_index = Filename.concat path "index" in

        lazy_seq [
          lazy (path_if_exists ~options path_acc path);
          lazy (path_if_exists_with_file_exts ~options path_acc path file_exts);
          lazy (path_if_exists_with_file_exts ~options path_acc path_w_index file_exts);
        ]

  let resolve_relative ~options cx loc ?path_acc root_path rel_path =
    let path = Files.normalize_path root_path rel_path in
    if Files.is_flow_file ~options path
    then path_if_exists ~options path_acc path
    else (
      let path_w_index = Filename.concat path "index" in
      (* We do not try resource file extensions here. So while you can write
       * require('foo') to require foo.js, it should never resolve to foo.css
       *)
      let file_exts = Options.module_file_exts options
        |> SSet.elements in

      lazy_seq ([
        lazy (path_if_exists_with_file_exts ~options path_acc path file_exts);
        lazy (parse_main ~options cx loc path_acc (Filename.concat path "package.json") file_exts);
        lazy (path_if_exists_with_file_exts ~options path_acc path_w_index file_exts);
      ])
    )

  let rec node_module ~options cx loc path_acc dir r =
    lazy_seq [
      lazy (
        lazy_seq (Options.node_resolver_dirnames options |> List.map (fun dirname ->
          lazy (resolve_relative
            ~options
            cx loc ?path_acc dir (spf "%s%s%s" dirname Filename.dir_sep r)
          )
        ))
      );

      lazy (
        let parent_dir = Filename.dirname dir in
        if dir = parent_dir then None
        else node_module ~options cx loc path_acc (Filename.dirname dir) r
      );
    ]

  let absolute r =
    Str.string_match Files.absolute_path r 0

  let explicitly_relative r =
    Str.string_match Files.current_dir_name r 0
    || Str.string_match Files.parent_dir_name r 0

  let resolve_import ~options cx loc ?path_acc import_str =
    let file = string_of_filename (Context.file cx) in
    let dir = Filename.dirname file in
    if explicitly_relative import_str || absolute import_str
    then resolve_relative ~options cx loc ?path_acc dir import_str
    else node_module ~options cx loc path_acc dir import_str

  let imported_module ~options cx loc ?path_acc import_str =
    let candidates = module_name_candidates ~options import_str in

    let rec choose_candidate = function
      | [] -> None
      | candidate :: candidates ->
        match resolve_import ~options cx loc ?path_acc candidate with
        | None -> choose_candidate candidates
        | Some _ as result -> result
    in
    match choose_candidate candidates with
    | Some str -> Modulename.Filename (Files.filename_from_string ~options str)
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
  let short_module_name_of = function
    | Loc.Builtins -> assert false
    | Loc.LibFile file
    | Loc.SourceFile file
    | Loc.JsonFile file
    | Loc.ResourceFile file ->
        Filename.basename file |> Filename.chop_extension

  let is_mock =
    let mock_path = Str.regexp ".*/__mocks__/.*" in
    function
    | Loc.Builtins -> false
    | Loc.LibFile file
    | Loc.SourceFile file
    | Loc.JsonFile file
    | Loc.ResourceFile file ->
        Str.string_match mock_path file 0

  let expand_project_root_token options str =
    let root = Path.to_string (Options.root options)
      |> Sys_utils.normalize_filename_dir_sep in
    str
      |> Str.split_delim FlowConfig.project_root_token
      |> String.concat root
      |> Str.regexp

  let is_haste_file options file =
    let matched_haste_paths_whitelist file = List.exists
      (fun r -> Str.string_match (expand_project_root_token options r) (string_of_filename file) 0)
      (Options.haste_paths_whitelist options) in
    let matched_haste_paths_blacklist file = List.exists
      (fun r -> Str.string_match (expand_project_root_token options r) (string_of_filename file) 0)
      (Options.haste_paths_blacklist options) in
    (matched_haste_paths_whitelist file) && not (matched_haste_paths_blacklist file)

  let haste_name options file =
    let reduce_name name (regexp, template) =
      Str.global_replace regexp template name
    in
    List.fold_left
      reduce_name
      (string_of_filename file)
      (Options.haste_name_reducers options)

  let rec exported_module options file info =
    if is_mock file
    then Modulename.String (short_module_name_of file)
    else if Options.haste_use_name_reducers options
    then
      if is_haste_file options file
      then Modulename.String (haste_name options file)
      else exported_non_haste_module options file
    else match Docblock.providesModule info with
      | Some m -> Modulename.String m
      | None ->
          (* If foo.js.flow doesn't have a @providesModule, then look at foo.js
           * and use its @providesModule instead *)
          exported_non_haste_module options file

  and exported_non_haste_module options file =
    if Loc.check_suffix file Files.flow_ext
    then
      let file_without_flow_ext = Loc.chop_suffix file Files.flow_ext in
      if Parsing_service_js.has_ast file_without_flow_ext
      then
        let info = Parsing_service_js.get_docblock_unsafe file_without_flow_ext in
        exported_module options file_without_flow_ext info
      else
        Modulename.Filename (file_without_flow_ext)
    else
      Modulename.Filename file

  let expanded_name r =
    match Str.split_delim (Str.regexp_string "/") r with
    | [] -> None
    | package_name::rest ->
        ReversePackageHeap.get package_name |> opt_map (fun package ->
          Files.construct_path package rest
        )

  (* similar to Node resolution, with possible special cases *)
  let resolve_import ~options cx loc ?path_acc r =
    let file = string_of_filename (Context.file cx) in
    lazy_seq [
      lazy (Node.resolve_import ~options cx loc ?path_acc r);
      lazy (match expanded_name r with
        | Some r ->
          Node.resolve_relative ~options cx loc ?path_acc (Filename.dirname file) r
        | None -> None
      );
    ]

  let imported_module ~options cx loc ?path_acc imported_name =
    let candidates = module_name_candidates ~options imported_name in

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

    match resolve_import ~options cx loc ?path_acc chosen_candidate with
    | Some name ->
        Modulename.Filename (Files.filename_from_string ~options name)
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

let module_system = ref None

(* TODO: is it premature optimization to memoize this? how bad is doing the
   Hashtbl.find each time? *)
let get_module_system opts =
  match !module_system with
  | Some system -> system
  | None ->
    let module M = (val (match Options.module_system opts with
    | Options.Node -> (module Node: MODULE_SYSTEM)
    | Options.Haste -> (module Haste: MODULE_SYSTEM)
    )) in
    let system = (module M : MODULE_SYSTEM) in
    module_system := Some system;
    system

let exported_module ~options file info =
  let module M = (val (get_module_system options)) in
  M.exported_module options file info

let imported_module ~options cx loc ?path_acc r =
  let module M = (val (get_module_system options)) in
  M.imported_module ~options cx loc ?path_acc r

let imported_modules ~options cx =
  (* Resolve all reqs relative to the given cx. Accumulate dependent paths in
     path_acc. Return the map of reqs to their resolved names, and the set
     containing the resolved names. *)
  let reqs = Context.required cx in
  let req_locs = Context.require_loc cx in
  let path_acc = ref SSet.empty in
  let set, map = SSet.fold (fun r (set, map) ->
    let loc = SMap.find_unsafe r req_locs in
    let resolved_r = imported_module ~options cx loc ~path_acc r in
    NameSet.add resolved_r set, SMap.add r resolved_r map
  ) reqs (NameSet.empty, SMap.empty) in
  set, map, !path_acc

(* Look up cached resolved module. *)
let cached_resolved_module ~audit file r =
  match get_resolved_requires ~audit file with
  | Some { resolved_modules; _ } -> SMap.get r resolved_modules
  | None -> None

(* Optimized module resolution function that goes through cache. *)
let find_resolved_module ~audit ~options cx loc r =
  let context_file = Context.file cx in
  match cached_resolved_module ~audit context_file r with
  | Some resolved_r -> resolved_r
  | None -> imported_module ~options cx loc r

let choose_provider ~options m files errmap =
  let module M = (val (get_module_system options)) in
  M.choose_provider m files errmap

(******************)
(***** public *****)
(******************)

let module_exists = NameHeap.mem

let get_file_unsafe ~audit m =
  match get_file ~audit m with
  | Some file -> file
  | None -> failwith
      (spf "file name not found for module %s" (Modulename.to_string m))

let get_resolved_requires_unsafe ~audit f =
  match get_resolved_requires ~audit f with
  | Some resolved_requires -> resolved_requires
  | None -> failwith
      (spf "resolved requires not found for file %s" (string_of_filename f))

let get_info_unsafe ~audit f =
  match get_info ~audit f with
  | Some info -> info
  | None -> failwith
      (spf "module info not found for file %s" (string_of_filename f))

(* TODO [perf]: measure size and possibly optimize *)
(* Extract and process information from context. In particular, resolve
   references to required modules in a file, and record the results.  *)
let resolved_requires_of ~options cx =
  let required, resolved_modules, phantom_dependents =
    imported_modules ~options cx in
  let require_loc = SMap.fold
    (fun r loc require_loc ->
      let resolved_r = SMap.find_unsafe r resolved_modules in
      SMap.add (Modulename.to_string resolved_r) loc require_loc)
    (Context.require_loc cx) SMap.empty in
  {
    required;
    require_loc;
    resolved_modules;
    phantom_dependents;
  }

(* Before and after inference, we add per-file module info and resolved requires
   to the shared heap from worker processes. Note that we wait to choose
   providers until inference is complete. *)
let add_parsed_info ~audit ~options file docblock =
  let force_check = Options.all options in
  let _module = exported_module ~options file docblock in
  let checked =
    force_check ||
    Docblock.is_flow docblock
  in
  add_info ~audit file {
    _module;
    checked;
    parsed = true;
  }

let add_parsed_resolved_requires ~audit ~options cx =
  let resolved_requires = resolved_requires_of ~options cx in
  let file = Context.file cx in
  add_resolved_requires ~audit file resolved_requires

(* We need to track files that have failed to parse. This begins with
   adding tracking records for unparsed files to InfoHeap. They never
   become providers - the process of committing modules happens after
   parsed files are finished with local inference. But since we guess
   the module names of unparsed files, we're able to tell whether an
   unparsed file has been required/imported.
 *)
let add_unparsed_info ~audit ~options file docblock =
  let force_check = Options.all options in
  let _module = exported_module ~options file docblock in
  let checked =
    force_check ||
    Loc.source_is_lib_file file ||
    Docblock.is_flow docblock ||
    Docblock.isDeclarationFile docblock
  in
  add_info ~audit file {
    _module;
    checked;
    parsed = false;
  }

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
(** TODO [perf]: investigate whether this takes too much memory **)
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

(* Pick providers for modules exported by new and changed files and for cleared
   modules (which cover changed and deleted files).

   For deleted files, their exported modules, if in cleared modules, will pick a
   new provider, or be left with no provider.

   For changed files, their exported modules, if in cleared modules, may pick
   the same provider (i.e., the changed file) or a new provider (a different
   file). If not in cleared modules, they may pick a new provider (i.e., the
   changed file) or the same provider (a different file).

   For new files, their exported modules may pick a new provider (i.e., the new
   file) or the same provider (a different file).

   parsed_unparsed is a list of parsed / unparsed file names.
   cleared_modules is a set of removed module names.

   Modules provided by parsed / unparsed files may or may not have a
   provider. Modules named in cleared_modules definitely do not have a
   provider. Together, they are considered "dirty" modules. Files that require
   dirty modules will be rechecked.

   Preconditions:
   1. all files in parsed_unparsed have entries in InfoHeap (true if
   we're properly calling add_parsed_info and add_unparsed_info for every
   parsed / unparsed file before calling commit_modules)
   2. all modules not mentioned in cleared_modules, but provided by one or more
   files in InfoHeap, have some provider registered in NameHeap.
   (However, the current provider may not be the one we now want,
   given newly parsed / unparsed files.)
   3. conversely all modules in cleared_modules lack a provider in NameHeap.

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
let calc_dirty_modules workers ~options parsed_unparsed cleared_modules =
  let debug = Options.is_debug_mode options in
  if debug then prerr_endlinef
    "*** committing modules: parsed / unparsed files %d removed modules %d ***"
    (List.length parsed_unparsed) (NameSet.cardinal cleared_modules);

  (* recall the exported module for each parsed / unparsed file *)
  let calc_file_module_assoc =
    List.fold_left (fun file_module_assoc f ->
      let { _module = m; _ } = get_info_unsafe ~audit:Expensive.ok f in
      (* [perf] using a list instead of a map *)
      (f, m) :: file_module_assoc
    ) in
  let file_module_assoc = MultiWorker.call
    workers
    ~job: calc_file_module_assoc
    ~neutral: []
    ~merge: List.rev_append
    ~next: (MultiWorker.next workers parsed_unparsed) in

  (* all removed modules must be repicked *)
  (* all modules provided by newly parsed / unparsed files must be repicked *)
  List.fold_left (fun acc (f, m) ->
    let f_module = Modulename.Filename f in
    add_provider f m; add_provider f f_module;
    (* foo.js.flow ALWAYS also provides foo.js *)
    if Loc.check_suffix f Files.flow_ext
    then begin
      let f_decl_module =
        Modulename.Filename (Loc.chop_suffix f Files.flow_ext) in
      add_provider f f_decl_module
    end;
    acc |> NameSet.add m |> NameSet.add f_module
  ) cleared_modules file_module_assoc

let commit_modules workers ~options dirty_modules =
  let debug = Options.is_debug_mode options in
  let module_files = MultiWorker.call
    workers
    ~job: (List.fold_left (fun acc m ->
      (m, get_file ~audit:Expensive.ok m)::acc
    ))
    ~neutral: []
    ~merge: List.rev_append
    ~next: (MultiWorker.next workers (NameSet.elements dirty_modules)) in
  (* prep for registering new mappings in NameHeap *)
  let mapping m p module_file_assoc =
    (m, p)::(
      let p_module = Modulename.Filename p in
      if p_module <> m
      then (p_module, p)::module_file_assoc
      else module_file_assoc
    ) in
  let remove, providers, replace, errmap = List.fold_left
    (fun (rem, prov, rep, errmap) (m, f_opt) ->
    match get_providers m with
    | ps when FilenameSet.is_empty ps ->
        if debug then prerr_endlinef
          "no remaining providers: %S"
          (Modulename.to_string m);
        (NameSet.add m rem), prov, rep, errmap
    | ps ->
      (* incremental: install empty error sets here for provider candidates.
         this will have the effect of resetting downstream errors for these
         files, when the returned error map is used by our caller.
         IMPORTANT: since each file may (does) provide more than one module,
         files may already have acquired errors earlier in this fold, so we
         must only add an empty entry if no entry is already present
      *)
      let errmap = FilenameSet.fold (fun f acc ->
        match FilenameMap.get f acc with
        | Some _ -> acc
        | None -> FilenameMap.add f [] acc
      ) ps errmap in
      (* now choose provider for m *)
      let p, errmap = choose_provider
        ~options (Modulename.to_string m) ps errmap in
      (* register chosen provider in NameHeap *)
      match f_opt with
      | Some f ->
        if f = p then begin
          (* When can this happen? Say m pointed to f before, a different file
             f' that provides m changed (so m is not in cleared_modules), but f
             continues to be the chosen provider = p (winning over f'). *)
          if debug then prerr_endlinef
            "unchanged provider: %S -> %s"
            (Modulename.to_string m)
            (string_of_filename p);
          rem, prov, rep, errmap
        end else begin
          (* When can this happen? Say m pointed to f before, a different file
             f' that provides m changed (so m is not in cleared_modules), and
             now f' becomes the chosen provider = p (winning over f). *)
          if debug then prerr_endlinef
            "new provider: %S -> %s replaces %s"
            (Modulename.to_string m)
            (string_of_filename p)
            (string_of_filename f);
          (NameSet.add m rem), p::prov, (mapping m p rep), errmap
        end
      | None ->
          (* When can this happen? Either m pointed to a file that used to
             provide m and changed or got deleted (causing m to be in
             cleared_modules), or m didn't have a provider before. *)
          if debug then prerr_endlinef
            "initial provider %S -> %s"
            (Modulename.to_string m)
            (string_of_filename p);
          rem, p::prov, (mapping m p rep), errmap
  ) (NameSet.empty, [], [], FilenameMap.empty) module_files in

  (* update NameHeap *)
  if not (NameSet.is_empty remove) then begin
    NameHeap.remove_batch remove;
    SharedMem_js.collect options `gentle;
  end;

  MultiWorker.call
    workers
    ~job: (fun () replace ->
      List.iter (fun (m, f) ->
        add_file Expensive.ok m f
      ) replace;
    )
    ~neutral: ()
    ~merge: (fun () () -> ())
    ~next: (MultiWorker.next workers replace);

  if debug then prerr_endlinef "*** done committing modules ***";
  providers, errmap

let remove_batch_resolved_requires files =
  ResolvedRequiresHeap.remove_batch files

(* Clear module mappings for given files, if they exist.

   This has no effect on new files. The set of modules returned are those whose
   current providers are changed or deleted files.

   As a side effect, we delete entries for the given files from InfoHeap and
   NameHeap, and moreover, clear the returned modules from NameHeap. Entries
   corresponding new and changed files are (re)populated in InfoHeap and
   NameHeap later.

   Possibilities:
   1. file is current registered module provider for a given module name
   2. file is not current provider, but record is still registered
   3. file isn't in the map at all. This means file is new.
   We return the set of module names whose current providers have been
   cleared (#1). This is the set commit_modules expects as its second
   argument.

   NOTE: The notion of "current provider" is murky, since every file at least
   provides its eponymous module. So we also include it in the returned set.

   TODO: Does a .flow file also provide its eponymous module? Or does it provide
   the eponymous module of the file it shadows?
*)
let clear_files options workers to_clear =
  let existing_file_module_current_provider_assoc = MultiWorker.call workers
    ~job: (List.fold_left (fun acc file ->
      match get_info ~audit:Expensive.ok file with
      | Some info ->
        let { _module; _ } = info in
        let current_provider = match get_file ~audit:Expensive.ok _module with
        | Some f when f = file -> true
        | _ -> false
        in
        FilenameMap.add file (_module, current_provider) acc
      | None -> acc
    ))
    ~neutral: FilenameMap.empty
    ~merge: FilenameMap.union
    ~next: (MultiWorker.next workers (FilenameSet.elements to_clear)) in

  (* files may or may not be registered as module providers.
     when they are, we need to clear their registrations *)
  let names = FilenameMap.fold (fun file datum names ->
    let _module, current_provider = datum in
    remove_provider file _module;
    remove_provider file (Modulename.Filename file);
    if current_provider then
      names
      |> NameSet.add _module
      |> NameSet.add (Modulename.Filename file)
    else
      names
      |> NameSet.add (Modulename.Filename file)
  ) existing_file_module_current_provider_assoc NameSet.empty in
  (* clear any registrations that point to records we're about to clear *)
  (* for records, remove_batch will ignore missing entries, no need to filter *)
  NameHeap.remove_batch names;
  InfoHeap.remove_batch to_clear;
  ResolvedRequiresHeap.remove_batch to_clear;
  SharedMem_js.collect options `gentle;
  (* note: only return names of modules actually removed *)
  names
