(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Code_action_text_edits

let main_of_package ~get_package_info package_dir =
  let file_key = File_key.JsonFile (package_dir ^ Filename.dir_sep ^ "package.json") in
  match get_package_info file_key with
  | Some (Ok package) -> Package_json.main package
  | Some (Error _)
  | None ->
    None

(** [find_ancestor_rev a_parts b_parts], where [a_parts] and [b_parts] are two paths split
    into segments (see [Files.split_path]), returns [(ancestor_parts, a_relative, b_relative)],
    where [ancestor_parts] are the common prefix parts **reversed**, [a_relative] is the
    remaining parts from the ancestor to [a_parts], and [b_relative] is the remaining parts
    from the ancestor to [b_parts].

    for example, [find_ancestor_rev ["/a"; "b"; "c"; "d"] ["/a"; "b"; "e"; "f"]] returns
    [(["b"; "/a"], ["c"; "d"], ["e"; "f"])] *)
let find_ancestor_rev =
  let rec helper acc = function
    | (dir1 :: rest1, dir2 :: rest2) when dir1 = dir2 -> helper (dir1 :: acc) (rest1, rest2)
    | (a_rel, b_rel) -> (acc, a_rel, b_rel)
  in
  (fun a_parts b_parts -> helper [] (a_parts, b_parts))

(** [path_matches expected actual] returns true if [actual] is the same as [expected], ignoring
    a potential leading [./] on [actual]. *)
let path_matches expected actual =
  expected = actual || (Filename.is_relative actual && actual = "./" ^ expected)

let string_of_path_parts parts =
  let str = String.concat "/" parts in
  let str' = String_utils.rstrip str "/index.js" in
  if str == str' then
    String_utils.rstrip str ".js"
  else
    str'

(** [node_path ~node_resolver_dirnames ~reader src_dir require_path] converts absolute path
    [require_path] into a Node-compatible "require" path relative to [src_dir], taking into
    account node's hierarchical search for [node_modules].

    That is, if [require_path] is within a [node_modules] folder in [src_dir] or one of
    [src_dir]'s parents, then the [node_modules] prefix is removed. If the package's
    [package.json] has a [main] field, that suffix is also removed.

    If not part of [node_modules], then [require_path] is relativized with respect to
    [src_dir].

    Lastly, if the path ends with [index.js] or [.js], those default suffixes are also
    removed. *)
let node_path ~node_resolver_dirnames ~get_package_info ~src_dir require_path =
  let require_path = String_utils.rstrip require_path Files.flow_ext in
  let src_parts = Files.split_path src_dir in
  let req_parts = Files.split_path require_path in
  let (ancestor_rev, to_src, to_req) = find_ancestor_rev src_parts req_parts in
  match to_req with
  | node_modules :: package_dir :: rest when List.mem node_modules node_resolver_dirnames ->
    let package_path =
      package_dir :: node_modules :: ancestor_rev |> Base.List.rev |> String.concat "/"
    in
    (match main_of_package ~get_package_info package_path with
    | Some main when path_matches (String.concat "/" rest) main -> package_dir
    | _ -> string_of_path_parts (package_dir :: rest))
  | _ ->
    let parts =
      if Base.List.is_empty to_src then
        Filename.current_dir_name :: to_req
      else
        (* add `..` for each dir in `to_src`, to relativize `to_req` *)
        Base.List.fold_left ~f:(fun path _ -> Filename.parent_dir_name :: path) ~init:to_req to_src
    in
    string_of_path_parts parts

(** [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
    suitable for importing [t] from a file in [src_dir]. that is, if it is a
    filename, returns the path relative to [src_dir]. *)
let path_of_modulename ~node_resolver_dirnames ~get_package_info src_dir file_key = function
  | Some _ as string_module_name -> string_module_name
  | None ->
    Base.Option.map
      ~f:(fun src_dir ->
        let path = File_key.to_string (Files.chop_flow_ext file_key) in
        node_path ~node_resolver_dirnames ~get_package_info ~src_dir path)
      src_dir

let haste_package_path ~get_package_info ~is_package_file ~src_dir require_path =
  match Files.split_path require_path |> Base.List.rev with
  | [] -> None
  | base :: parent_dir_names ->
    let src_parts = Files.split_path src_dir in
    let rec f acc remaining =
      match remaining with
      | [] -> None
      | package_name_candidate :: parent_dir_names ->
        if is_package_file package_name_candidate then
          let package_path_parts = List.rev (package_name_candidate :: parent_dir_names) in
          let within_package =
            match find_ancestor_rev package_path_parts src_parts with
            (* src is completely within package_path if they have a common ancestor,
               and additional relative path required to get to package path is empty. *)
            | (_, [], _) -> true
            | _ -> false
          in
          if within_package then
            None
          else
            Some
              (match
                 main_of_package
                   ~get_package_info
                   (String.concat Filename.dir_sep package_path_parts)
               with
              | Some main when path_matches (String.concat "/" acc) main -> package_name_candidate
              | _ -> string_of_path_parts (package_name_candidate :: acc))
        else
          f (package_name_candidate :: acc) parent_dir_names
    in
    f [base] parent_dir_names

let from_of_source
    ~file_options
    ~haste_module_system
    ~get_haste_name
    ~get_package_info
    ~is_package_file
    ~src_dir
    source =
  match source with
  | Export_index.Global -> None
  | Export_index.Builtin from -> Some from
  | Export_index.File_key from ->
    let module_name =
      match get_haste_name from with
      | Some module_name -> Some module_name
      | None when haste_module_system ->
        Base.Option.bind src_dir ~f:(fun src_dir ->
            haste_package_path
              ~get_package_info
              ~is_package_file
              ~src_dir
              (File_key.to_string (Files.chop_flow_ext from))
        )
      | None -> None
    in
    let node_resolver_dirnames = Files.node_resolver_dirnames file_options in
    path_of_modulename ~node_resolver_dirnames ~get_package_info src_dir from module_name

let text_edits_of_import
    ~file_options
    ~layout_options
    ~haste_module_system
    ~get_haste_name
    ~get_package_info
    ~is_package_file
    ~src_dir
    ~ast
    kind
    name
    source =
  let from =
    from_of_source
      ~file_options
      ~haste_module_system
      ~get_haste_name
      ~get_package_info
      ~is_package_file
      ~src_dir
      source
  in
  match from with
  | None -> None
  | Some from ->
    let title =
      match kind with
      | Export_index.DefaultType -> Printf.sprintf "Import default type from %s" from
      | Export_index.Default -> Printf.sprintf "Import default from %s" from
      | Export_index.Named -> Printf.sprintf "Import from %s" from
      | Export_index.NamedType -> Printf.sprintf "Import type from %s" from
      | Export_index.Namespace -> Printf.sprintf "Import * from %s" from
    in
    let bindings =
      match kind with
      | Export_index.DefaultType -> Autofix_imports.DefaultType name
      | Export_index.Default -> Autofix_imports.Default name
      | Export_index.Named ->
        Autofix_imports.Named [{ Autofix_imports.remote_name = name; local_name = None }]
      | Export_index.NamedType ->
        Autofix_imports.NamedType [{ Autofix_imports.remote_name = name; local_name = None }]
      | Export_index.Namespace -> Autofix_imports.Namespace name
    in
    let edits =
      Autofix_imports.add_import ~options:layout_options ~bindings ~from ast
      |> Base.List.map ~f:(fun (loc, text) ->
             { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText = text }
         )
    in
    Some { title; edits; from }

module For_tests = struct
  let path_of_modulename = path_of_modulename
end
