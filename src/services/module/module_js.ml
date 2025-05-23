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
  fun ~options m errmap providers fallback ->
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
    (* If both a definition and an implementation exist, choose between them. A
     * definition only shadows the implementation with the same path, otherwise
     * they are considered distinct providers. *)
    | (impl :: dup_impls, defn :: dup_defns) ->
      let (impl_key, _) = impl in
      let (defn_key, _) = defn in
      let def_with_flow_ext_chopped = Files.chop_flow_ext defn_key in
      let impl_with_platform_suffix_chopped =
        Files.chop_platform_suffix_for_file ~options:(Options.file_options options) impl_key
      in
      let errmap =
        (* Allow pair of A.js.flow & A.ios.js *)
        if def_with_flow_ext_chopped = impl_with_platform_suffix_chopped then
          errmap
        else if
          (* Additionally allow pair of A.ios.js.flow & A.ios.js *)
          Files.chop_platform_suffix_for_file
            ~options:(Options.file_options options)
            def_with_flow_ext_chopped
          = impl_with_platform_suffix_chopped
        then
          errmap
        else
          (* For illegal shadow errors, we error on the pair .js.flow and .js *)
          warn_duplicate_providers m defn [impl] errmap
      in
      let errmap =
        errmap
        (* For duplicate provider errors, we error on either two implementation files
         * or on two declaration files *)
        |> warn_duplicate_providers m impl dup_impls
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
let module_name_candidates ~options name =
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
  Nel.rev (name, List.fold_left map_name [] mappers)

type package_incompatible_reason =
  | New  (** Didn't exist before, now it exists *)
  | Became_invalid  (** Was valid, now is invalid *)
  | Became_valid  (** Was invalid, now is valid *)
  | Name_changed of string option * string option
      (** The `name` property changed from the former to the latter *)
  | Main_changed of string option * string option
      (** The `main` property changed from the former to the latter *)
  | Haste_commonjs_changed of bool  (** The `haste_commonjs` property changed to this value *)
  | Exports_changed  (** The `exports` property changed *)
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
  | Exports_changed -> "exports changed"
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
      let old_exports = Package_json.exports old_package in
      let new_exports = Package_json.exports new_package in
      if old_name <> new_name then
        Incompatible (Name_changed (old_name, new_name))
      else if old_main <> new_main then
        Incompatible (Main_changed (old_main, new_main))
      else if old_haste_commonjs <> new_haste_commonjs then
        Incompatible (Haste_commonjs_changed new_haste_commonjs)
      else if old_exports <> new_exports then
        Incompatible Exports_changed
      else
        (* This shouldn't happen -- if it does, it probably means we need to add cases above *)
        Incompatible Unknown

type phantom_acc = Parsing_heaps.dependency_addr option Modulename.Map.t ref

type package_info = Package_json.t option

(* Specification of a module system. Currently this signature is sufficient to
   model both Haste and Node, but should be further generalized. *)
module type MODULE_SYSTEM = sig
  (* Given a file and docblock info, make the name of the module it exports. *)
  val exported_module :
    Options.t -> File_key.t -> package_info:package_info -> Haste_module_info.t option

  (* Given a file (importing_file) and a reference in it to an imported module (import_specifier),
     make the name of the module it refers to. If given an optional reference to an accumulator,
     record paths that were looked up but not found during resolution. *)
  val imported_module :
    options:Options.t ->
    reader:Abstract_state_reader.t ->
    node_modules_containers:SSet.t SMap.t ->
    importing_file:File_key.t ->
    phantom_acc:phantom_acc option ->
    Flow_import_specifier.t ->
    Parsing_heaps.dependency_addr Parsing_heaps.resolved_module'

  (* for a given module name, choose a provider from among a set of
     files with that exported name. also check for duplicates and
     generate warnings, as dictated by module system rules. *)
  val choose_provider :
    options:Options.t ->
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

let resolve_symlinks path = File_path.to_string (File_path.make path)

let record_phantom_dependency mname dependency = function
  | None -> ()
  | Some phantom_acc -> phantom_acc := Modulename.Map.add mname dependency !phantom_acc

(****************** Node module system *********************)

module Node = struct
  let exported_module _ _ ~package_info:_ = None

  let path_if_exists ~reader ~file_options ~phantom_acc path =
    let path = resolve_symlinks path in
    let mname =
      Files.eponymous_module
        (Files.filename_from_string
           ~options:file_options
           ~consider_libdefs:false (* Module resolution should never resolve to a libdef file *)
           ~all_unordered_libs:SSet.empty
           path
        )
    in
    let dependency = Parsing_heaps.get_dependency mname in
    match Option.bind dependency (Parsing_heaps.Reader_dispatcher.get_provider ~reader) with
    | Some _ -> dependency
    | None ->
      record_phantom_dependency mname dependency phantom_acc;
      None

  let path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc path file_exts =
    let f ext = path_if_exists ~reader ~file_options ~phantom_acc (path ^ ext) in
    List.find_map f file_exts

  let parse_package ~reader package_filename =
    let package_filename = resolve_symlinks package_filename in
    let file_key = File_key.JsonFile package_filename in
    match Parsing_heaps.Reader_dispatcher.get_package_info ~reader file_key with
    | Some (Ok package) -> package
    | Some (Error ()) ->
      (* invalid, but we already raised an error when building PackageHeap *)
      Package_json.empty
    | None -> Package_json.empty

  let parse_exports ~reader ~options phantom_acc package_dir subpath file_exts =
    let subpath =
      match subpath with
      | Some s -> s
      | None -> "."
    in
    let file_options = Options.file_options options in
    let export_conditions = Options.node_package_export_conditions options in
    let package = parse_package ~reader (Filename.concat package_dir "package.json") in
    let source_path =
      match Package_json.exports package with
      | None -> None
      | Some exports -> Package_exports.resolve_package exports subpath export_conditions
    in
    match source_path with
    | None -> None
    | Some file ->
      let path = Files.normalize_path package_dir file in
      lazy_seq
        [
          lazy (path_if_exists ~reader ~file_options ~phantom_acc path);
          lazy (path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc path file_exts);
        ]

  let parse_main ~reader ~file_options phantom_acc package_filename file_exts =
    let package = parse_package ~reader package_filename in
    match Package_json.main package with
    | None -> None
    | Some file ->
      let dir = Filename.dirname package_filename in
      let path = Files.normalize_path dir file in
      let path_w_index = Filename.concat path "index" in
      lazy_seq
        [
          lazy (path_if_exists ~reader ~file_options ~phantom_acc path);
          lazy (path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc path file_exts);
          lazy
            (path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc path_w_index file_exts);
        ]

  let resolve_package ~options ~reader ~phantom_acc ~subpath package_dir =
    let file_options = Options.file_options options in
    let file_exts = Files.module_file_exts file_options in
    let full_package_path =
      match subpath with
      | Some s -> Files.normalize_path package_dir s
      | None -> package_dir
    in
    lazy_seq
      [
        lazy (parse_exports ~reader ~options phantom_acc package_dir subpath file_exts);
        lazy
          (parse_main
             ~reader
             ~file_options
             phantom_acc
             (Filename.concat full_package_path "package.json")
             file_exts
          );
        lazy
          (path_if_exists_with_file_exts
             ~reader
             ~file_options
             ~phantom_acc
             (Filename.concat full_package_path "index")
             file_exts
          );
      ]

  let ordered_allowed_implicit_platform_specific_import
      ~file_options ~projects_options ~importing_file =
    match
      Platform_set.available_platforms
        ~file_options
        ~projects_options
        ~filename:importing_file
        ~explicit_available_platforms:None
      |> Option.map (Platform_set.to_platform_string_set ~file_options)
    with
    | None -> None
    | Some available_platform_set ->
      (* For .ios.js files, we will try .ios platform import.
       * If .native contains .ios, we will try .native imports as well. *)
      let single_platform =
        if SSet.cardinal available_platform_set = 1 then
          [SSet.choose available_platform_set]
        else
          []
      in
      (* If the file's available platform set matches a platform extension group, then
       * we also allow import for that extension group.
       * e.g. .native files can implicitly import other .native files without .native. *)
      let grouped_platform =
        Base.List.find_map
          (Files.multi_platform_extension_group_mapping file_options)
          ~f:(fun (group_ext, platforms) ->
            if SSet.subset available_platform_set (SSet.of_list platforms) then
              Some (Base.String.chop_prefix_exn ~prefix:"." group_ext)
            else
              None
        )
        |> Option.to_list
      in
      Some (single_platform @ grouped_platform)

  let resolve_relative
      ~options ~reader ~phantom_acc ~importing_file ~relative_to_directory ?subpath rel_path =
    let file_options = Options.file_options options in
    let projects_options = Options.projects_options options in
    let package_path = Files.normalize_path relative_to_directory rel_path in
    let full_path =
      match subpath with
      | Some s -> Files.normalize_path package_path s
      | None -> package_path
    in
    (* We do not try resource file extensions here. So while you can write
     * require('foo') to require foo.js, it should never resolve to foo.css
     *)
    let file_exts = Files.module_file_exts file_options in
    match
      ordered_allowed_implicit_platform_specific_import
        ~file_options
        ~projects_options
        ~importing_file:(File_key.to_string importing_file)
    with
    | None ->
      lazy_seq
        [
          (* Try <path> import directly. Needed for `import './foo.js'`  *)
          lazy (path_if_exists ~reader ~file_options ~phantom_acc full_path);
          (* Try <path>.js import. Needed for `import './foo'`  *)
          lazy (path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc full_path file_exts);
          lazy (resolve_package ~options ~reader ~phantom_acc ~subpath package_path);
        ]
    | Some ordered_platforms ->
      lazy_seq
        ([
           (* Try <path> import directly. Needed for `import './foo.js'`  *)
           lazy (path_if_exists ~reader ~file_options ~phantom_acc full_path);
         ]
         (* Try <path>.<platform>.js import.
          * Needed so that `import './foo'` resolves to foo.android.js in android.js files *)
        @ Base.List.map ordered_platforms ~f:(fun platform ->
              lazy
                (path_if_exists_with_file_exts
                   ~reader
                   ~file_options
                   ~phantom_acc
                   (full_path ^ "." ^ platform)
                   file_exts
                )
          )
        @ [
            (* Try <path>.js import. Needed for `import './foo'`  *)
            lazy
              (path_if_exists_with_file_exts ~reader ~file_options ~phantom_acc full_path file_exts);
            lazy (resolve_package ~options ~reader ~phantom_acc ~subpath package_path);
          ]
        )

  (* Parses a package import specifier into a package name and subpath,
   * accounting for things such as scoped packages *)
  let parse_package_name specifier =
    match specifier with
    | "" -> ("", ".")
    | _ ->
      let initial_char = String.sub specifier 0 1 in
      let first_seperator_index =
        try Str.search_forward (Str.regexp_string Filename.dir_sep) specifier 0 with
        | Not_found -> -1
      in
      let seperator_index =
        match initial_char with
        | "@" ->
          (try
             Str.search_forward
               (Str.regexp_string Filename.dir_sep)
               specifier
               (first_seperator_index + 1)
           with
          | Not_found -> -1)
        | _ -> first_seperator_index
      in
      let package_name =
        match seperator_index with
        | -1 -> specifier
        | _ -> String.sub specifier 0 seperator_index
      in
      let package_subpath =
        "."
        ^
        match seperator_index with
        | -1 -> ""
        | _ -> Str.string_after specifier seperator_index
      in
      (package_name, package_subpath)

  let rec node_module
      ~options
      ~reader
      ~node_modules_containers
      ~phantom_acc
      ~importing_file
      ~possible_node_module_container_dir
      ~import_specifier =
    let file_options = Options.file_options options in
    let (package_name, package_subpath) = parse_package_name import_specifier in
    lazy_seq
      [
        lazy
          (match SMap.find_opt possible_node_module_container_dir node_modules_containers with
          | Some existing_node_modules_dirs ->
            lazy_seq
              (Files.node_resolver_dirnames file_options
              |> Base.List.map ~f:(fun dirname ->
                     lazy
                       ( if SSet.mem dirname existing_node_modules_dirs then
                         resolve_relative
                           ~options
                           ~reader
                           ~phantom_acc
                           ~importing_file
                           ~relative_to_directory:possible_node_module_container_dir
                           ~subpath:package_subpath
                           (spf "%s%s%s" dirname Filename.dir_sep package_name)
                       else
                         None
                       )
                 )
              )
          | None -> None);
        lazy
          (let parent_dir = Filename.dirname possible_node_module_container_dir in
           if possible_node_module_container_dir = parent_dir then
             None
           else
             node_module
               ~options
               ~reader
               ~node_modules_containers
               ~phantom_acc
               ~importing_file
               ~possible_node_module_container_dir:
                 (Filename.dirname possible_node_module_container_dir)
               ~import_specifier
          );
      ]

  let flow_typed_module ~options ~reader ~phantom_acc ~importing_file ~import_specifier =
    Options.file_options options
    |> Files.module_declaration_dirnames
    |> Base.List.map ~f:(fun relative_to_directory ->
           lazy
             (resolve_relative
                ~options
                ~reader
                ~phantom_acc
                ~importing_file
                ~relative_to_directory
                ~subpath:"."
                import_specifier
             )
       )
    |> lazy_seq

  (* The flowconfig option `module.system.node.allow_root_relative` tells Flow
   * to resolve requires like `require('foo/bar.js')` relative to the project
   * root directory. This is something bundlers like Webpack can be configured
   * to do. *)
  let resolve_root_relative ~options ~reader ~phantom_acc ~importing_file ~import_specifier =
    if Options.node_resolver_allow_root_relative options then
      let dirnames = Options.node_resolver_root_relative_dirnames options in
      let root = Options.root options |> File_path.to_string in
      let f (applicable_dirname_opt, dirname) =
        let relative_to_directory =
          if dirname = "" then
            root
          else
            Files.normalize_path root dirname
        in
        let applicable =
          Base.Option.value_map
            applicable_dirname_opt
            ~f:(fun prefix -> Files.is_prefix prefix (File_key.to_string importing_file))
            ~default:true
        in
        if applicable then
          Some
            ( lazy
              (resolve_relative
                 ~options
                 ~reader
                 ~phantom_acc
                 ~importing_file
                 ~relative_to_directory
                 import_specifier
              )
              )
        else
          None
      in
      lazy_seq (Base.List.filter_map ~f dirnames)
    else
      None

  let resolve_import
      ~options ~reader ~node_modules_containers ~importing_file ~phantom_acc import_specifier =
    let file = File_key.to_string importing_file in
    let importing_file_dir = Filename.dirname file in
    if is_relative_or_absolute import_specifier then
      resolve_relative
        ~options
        ~reader
        ~phantom_acc
        ~importing_file
        ~relative_to_directory:importing_file_dir
        import_specifier
    else
      lazy_seq
        [
          lazy
            (resolve_root_relative ~options ~reader ~phantom_acc ~importing_file ~import_specifier);
          lazy (flow_typed_module ~options ~reader ~phantom_acc ~importing_file ~import_specifier);
          lazy
            (node_module
               ~options
               ~reader
               ~node_modules_containers
               ~phantom_acc
               ~importing_file
               ~possible_node_module_container_dir:importing_file_dir
               ~import_specifier
            );
        ]

  let imported_module ~options ~reader ~node_modules_containers ~importing_file ~phantom_acc =
    function
    | Flow_import_specifier.Userland import_specifier ->
      let candidates =
        module_name_candidates ~options (Flow_import_specifier.unwrap_userland import_specifier)
      in
      (match
         List.find_map
           (resolve_import ~options ~reader ~node_modules_containers ~importing_file ~phantom_acc)
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
        Error None)
    | Flow_import_specifier.HasteImportWithSpecifiedNamespace _ ->
      (* We should never find Haste modules under Node. *)
      Error None

  (* in node, file names are module names, as guaranteed by
     our implementation of exported_name, so anything but a
     singleton provider set is craziness. *)
  let choose_provider ~options m files errmap =
    let fallback () = None in
    choose_provider_and_warn_about_duplicates ~options m errmap files fallback
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

  let is_within_node_modules options =
    let root = Options.root options in
    let options = Options.file_options options in
    Files.is_within_node_modules ~root ~options

  let exported_module options =
    let haste_name_opt = Files.haste_name_opt ~options:(Options.file_options options) in
    let projects_options = Options.projects_options options in
    let namespace_of_path path =
      match path |> Flow_projects.projects_bitset_of_path ~opts:projects_options with
      | None -> failwith ("Path " ^ path ^ " doesn't match any Haste namespace.")
      | Some bitset -> Flow_projects.to_bitset bitset
    in
    let is_within_node_modules = is_within_node_modules options in
    fun file ~package_info ->
      match file with
      | File_key.SourceFile path ->
        if is_mock file then
          Some
            (Haste_module_info.mk
               ~module_name:(short_module_name_of file)
               ~namespace_bitset:(namespace_of_path path)
            )
        else (
          match haste_name_opt file with
          | Some module_name ->
            Some (Haste_module_info.mk ~module_name ~namespace_bitset:(namespace_of_path path))
          | None -> None
        )
      | File_key.JsonFile path ->
        (match package_info with
        | Some pkg when Package_json.haste_commonjs pkg || not (is_within_node_modules path) ->
          Package_json.name pkg
          |> Option.map (fun module_name ->
                 Haste_module_info.mk ~module_name ~namespace_bitset:(namespace_of_path path)
             )
        | _ -> None)
      | _ ->
        (* Lib files, resource files, etc don't have any fancy haste name *)
        None

  let package_dir_opt ~reader addr =
    if Parsing_heaps.Reader_dispatcher.is_package_file ~reader addr then
      Some (Parsing_heaps.read_file_name addr |> Filename.dirname)
    else
      None

  let resolve_haste_module
      ~options
      ~reader
      ~phantom_acc
      ~importing_file
      ~importing_file_dir
      ~namespace_opt
      ~import_specifier =
    let (name, subpath) =
      match String.split_on_char '/' import_specifier with
      | [] -> (import_specifier, [])
      | scope :: package :: rest when String.starts_with ~prefix:"@" scope ->
        (scope ^ "/" ^ package, rest)
      | package :: rest -> (package, rest)
    in
    let resolve mname =
      let dependency = Parsing_heaps.get_dependency mname in
      match Option.bind dependency (Parsing_heaps.Reader_dispatcher.get_provider ~reader) with
      | Some addr ->
        (match (package_dir_opt ~reader addr, subpath) with
        | (Some package_dir, []) ->
          Node.resolve_package ~options ~reader ~phantom_acc ~subpath:None package_dir
        | (Some package_dir, subpath) ->
          (* add a phantom dep on the package name, so we re-resolve the subpath
             if the package gets a new provider *)
          record_phantom_dependency mname dependency phantom_acc;

          let path = Files.construct_path package_dir subpath in
          Node.resolve_relative
            ~options
            ~reader
            ~phantom_acc
            ~importing_file
            ~relative_to_directory:importing_file_dir
            path
        | (None, []) -> dependency
        | (None, _ :: _) ->
          (* if r = foo/bar and foo is a regular module, don't resolve.
             TODO: could we provide a better error than just failing to resolve?

             we do need to add a phantom dep on the module, so we re-resolve
             if the provider changes to a package. *)
          record_phantom_dependency mname dependency phantom_acc;
          None)
      | None ->
        record_phantom_dependency mname dependency phantom_acc;
        None
    in
    let haste_namespace_bitset_candidates =
      match namespace_opt with
      | Some namespace -> [namespace]
      | None ->
        let opts = Options.projects_options options in
        (match Flow_projects.projects_bitset_of_path ~opts (File_key.to_string importing_file) with
        | None -> []
        | Some bitset ->
          bitset
          |> Flow_projects.reachable_projects_bitsets_from_projects_bitset ~opts
          |> List.map Flow_projects.to_bitset)
    in
    let mname_of_bitset namespace_bitset =
      Modulename.Haste (Haste_module_info.mk ~module_name:name ~namespace_bitset)
    in
    lazy_seq
      (Base.List.map haste_namespace_bitset_candidates ~f:(fun b ->
           lazy (resolve (mname_of_bitset b))
       )
      )

  let resolve_haste_module_disallow_platform_specific_haste_modules_under_multiplatform
      ~options
      ~reader
      ~phantom_acc
      ~importing_file
      ~importing_file_dir
      ~namespace_opt
      ~import_specifier =
    let dependency =
      resolve_haste_module
        ~options
        ~reader
        ~phantom_acc
        ~importing_file
        ~importing_file_dir
        ~namespace_opt
        ~import_specifier
    in
    let file_options = Options.file_options options in
    if Files.multi_platform file_options then
      match Option.map Parsing_heaps.read_dependency dependency with
      | Some (Modulename.Haste haste_module_info as mname)
        when Base.List.exists (Files.multi_platform_extensions file_options) ~f:(fun ext ->
                 String.ends_with ~suffix:ext (Haste_module_info.module_name haste_module_info)
             ) ->
        (* If we don't allow an import to resolve a platform specific import, but we did find one,
           we should fail to resolve. This restriction only applies to Haste modules because Metro
           cannot resolve them.
           TODO: could we provide a better error than just failing to resolve? *)
        record_phantom_dependency mname dependency phantom_acc;
        None
      | _ -> dependency
    else
      dependency

  let resolve_import
      ~options ~reader ~node_modules_containers ~importing_file ~phantom_acc ~import_specifier =
    let file = File_key.to_string importing_file in
    let importing_file_dir = Filename.dirname file in
    if is_relative_or_absolute import_specifier then
      Node.resolve_relative
        ~options
        ~reader
        ~phantom_acc
        ~importing_file
        ~relative_to_directory:importing_file_dir
        import_specifier
    else
      let file_options = Options.file_options options in
      let projects_options = Options.projects_options options in
      if not (Files.multi_platform file_options) then
        lazy_seq
          [
            lazy
              (resolve_haste_module
                 ~options
                 ~reader
                 ~phantom_acc
                 ~importing_file
                 ~importing_file_dir
                 ~namespace_opt:None
                 ~import_specifier
              );
            lazy
              (Node.resolve_root_relative
                 ~options
                 ~reader
                 ~phantom_acc
                 ~importing_file
                 ~import_specifier
              );
            lazy
              (Node.flow_typed_module
                 ~options
                 ~reader
                 ~phantom_acc
                 ~importing_file
                 ~import_specifier
              );
            lazy
              (Node.node_module
                 ~options
                 ~reader
                 ~node_modules_containers
                 ~phantom_acc
                 ~importing_file
                 ~possible_node_module_container_dir:importing_file_dir
                 ~import_specifier
              );
          ]
      else
        match
          Node.ordered_allowed_implicit_platform_specific_import
            ~file_options
            ~projects_options
            ~importing_file:file
        with
        | None ->
          lazy_seq
            [
              lazy
                (resolve_haste_module_disallow_platform_specific_haste_modules_under_multiplatform
                   ~options
                   ~reader
                   ~phantom_acc
                   ~importing_file
                   ~importing_file_dir
                   ~namespace_opt:None
                   ~import_specifier
                );
              lazy
                (Node.resolve_root_relative
                   ~options
                   ~reader
                   ~phantom_acc
                   ~importing_file
                   ~import_specifier
                );
              lazy
                (Node.flow_typed_module
                   ~options
                   ~reader
                   ~phantom_acc
                   ~importing_file
                   ~import_specifier
                );
              lazy
                (Node.node_module
                   ~options
                   ~reader
                   ~node_modules_containers
                   ~phantom_acc
                   ~importing_file
                   ~possible_node_module_container_dir:importing_file_dir
                   ~import_specifier
                );
            ]
        | Some ordered_platforms ->
          lazy_seq
            (Base.List.map ordered_platforms ~f:(fun platform ->
                 lazy
                   (resolve_haste_module
                      ~options
                      ~reader
                      ~phantom_acc
                      ~importing_file
                      ~importing_file_dir
                      ~namespace_opt:None
                      ~import_specifier:(import_specifier ^ "." ^ platform)
                   )
             )
            @ [
                lazy
                  (resolve_haste_module_disallow_platform_specific_haste_modules_under_multiplatform
                     ~options
                     ~reader
                     ~phantom_acc
                     ~importing_file
                     ~importing_file_dir
                     ~namespace_opt:None
                     ~import_specifier
                  );
                lazy
                  (Node.resolve_root_relative
                     ~options
                     ~reader
                     ~phantom_acc
                     ~importing_file
                     ~import_specifier
                  );
                lazy
                  (Node.flow_typed_module
                     ~options
                     ~reader
                     ~phantom_acc
                     ~importing_file
                     ~import_specifier
                  );
                lazy
                  (Node.node_module
                     ~options
                     ~reader
                     ~node_modules_containers
                     ~phantom_acc
                     ~importing_file
                     ~possible_node_module_container_dir:importing_file_dir
                     ~import_specifier
                  );
              ]
            )

  let imported_module ~options ~reader ~node_modules_containers ~importing_file ~phantom_acc =
    function
    | Flow_import_specifier.Userland import_specifier ->
      (* For historical reasons, the Haste module system always picks the first
       * matching candidate, unlike the Node module system which picks the first
       * "valid" matching candidate. *)
      let candidates =
        module_name_candidates ~options (Flow_import_specifier.unwrap_userland import_specifier)
      in
      let import_specifier = Nel.hd candidates in
      (match
         resolve_import
           ~options
           ~reader
           ~node_modules_containers
           ~importing_file
           ~phantom_acc
           ~import_specifier
       with
      | Some m -> Ok m
      | None ->
        (* If the candidates list is a singleton, then no name mappers applied,
         * and we failed to resolve the unmapped name. Otherwise, `r` is the
         * chosen mapped name and we store it for error reporting. *)
        let mapped_name =
          match Nel.tl candidates with
          | [] -> None
          | _ -> Some (Flow_import_specifier.userland_specifier import_specifier)
        in
        Error mapped_name)
    | Flow_import_specifier.HasteImportWithSpecifiedNamespace { namespace; name } as specifier ->
      let importing_file_dir = Filename.dirname (File_key.to_string importing_file) in
      let import_specifier = Nel.hd (module_name_candidates ~options name) in
      (match
         resolve_haste_module
           ~options
           ~reader
           ~phantom_acc
           ~importing_file
           ~importing_file_dir
           ~namespace_opt:(Some namespace)
           ~import_specifier
       with
      | Some m -> Ok m
      | None -> Error (Some specifier))

  (* in haste, many files may provide the same module. here we're also
     supporting the notion of mock modules - allowed duplicates used as
     fallbacks. we prefer the non-mock if it exists, otherwise choose an
     arbitrary mock, if any exist. if multiple non-mock providers exist,
     we pick one arbitrarily and issue duplicate module warnings for the
     rest. *)
  let choose_provider ~options m files errmap =
    match files with
    | [] -> (None, errmap)
    | [(_, p)] -> (Some p, errmap)
    | files ->
      let (mocks, non_mocks) =
        let f (key, _) = is_mock key in
        List.partition f files
      in
      let fallback () = Some (snd (List.hd mocks)) in
      choose_provider_and_warn_about_duplicates ~options m errmap non_mocks fallback
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

let imported_module
    ~options ~reader ~node_modules_containers ~importing_file ?phantom_acc import_specifier =
  let module M = (val get_module_system options) in
  M.imported_module
    ~options
    ~reader
    ~node_modules_containers
    ~importing_file
    ~phantom_acc
    import_specifier

let choose_provider ~options m files errmap =
  let module M = (val get_module_system options) in
  M.choose_provider ~options m files errmap

(******************)
(***** public *****)
(******************)

let add_parsed_resolved_requires ~mutator ~reader ~options ~node_modules_containers file =
  let file_addr = Parsing_heaps.get_file_addr_unsafe file in
  let parse = Parsing_heaps.Mutator_reader.get_typed_parse_unsafe ~reader file file_addr in
  let requires = Parsing_heaps.read_requires parse in
  let phantom_acc = ref Modulename.Map.empty in
  let resolved_modules =
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    Array.map
      (fun mref ->
        imported_module
          ~importing_file:file
          mref
          ~options
          ~reader
          ~node_modules_containers
          ~phantom_acc)
      requires
  in
  Parsing_heaps.Resolved_requires_mutator.add_resolved_requires
    mutator
    file_addr
    parse
    resolved_modules
    !phantom_acc

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
  let commit_haste (unchanged, errmap) mname haste_module_info =
    let name = Haste_module_info.module_name haste_module_info in
    let m = Parsing_heaps.get_haste_module_unsafe haste_module_info in
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
  let job ((unchanged, errmap) as acc) mname =
    match mname with
    | Modulename.Haste name -> commit_haste acc mname name
    | Modulename.Filename key -> (commit_file unchanged mname key, errmap)
  in
  let%lwt (unchanged, duplicate_providers) =
    MultiWorkerLwt.fold
      workers
      ~job
      ~neutral:(Modulename.Set.empty, SMap.empty)
      ~merge:(fun (a1, a2) (b1, b2) -> (Modulename.Set.union a1 b1, SMap.union a2 b2))
      ~next:(MultiWorkerLwt.next workers (Modulename.Set.elements dirty_modules))
  in
  let changed_modules = Modulename.Set.diff dirty_modules unchanged in
  if debug then prerr_endlinef "*** done committing modules ***";
  Lwt.return (changed_modules, duplicate_providers)
