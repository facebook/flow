(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let autofix_exports_code_actions
    ~full_cx ~ast ~file_sig ~tolerable_errors ~typed_ast ~diagnostics uri loc =
  let open Lsp in
  let open Autofix_exports in
  let fixable_locs = set_of_fixable_signature_verification_locations tolerable_errors in
  if LocSet.mem loc fixable_locs then
    match fix_signature_verification_error_at_loc ~full_cx ~file_sig ~typed_ast ast loc with
    | new_ast ->
      let diff = Insert_type.mk_diff ast new_ast in
      let edits =
        Replacement_printer.mk_loc_patch_ast_differ diff
        |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
      in
      [
        CodeAction.Action
          {
            CodeAction.title = "insert type annotation";
            kind = CodeActionKind.quickfix;
            (* Handing back the diagnostics we were given is a placeholder for
               eventually generating the diagnostics for the errors we are fixing *)
            diagnostics;
            action = CodeAction.EditOnly WorkspaceEdit.{ changes = UriMap.singleton uri edits };
          };
      ]
  else
    []

let create_suggestion ~diagnostics ~original ~suggestion uri loc =
  let open Lsp in
  let title = Printf.sprintf "Replace %s with `%s`" original suggestion in
  let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
  let relevant_diagnostics =
    diagnostics |> List.filter (fun PublishDiagnostics.{ range; _ } -> range = error_range)
  in
  let textEdit = TextEdit.{ range = error_range; newText = suggestion } in
  CodeAction.Action
    CodeAction.
      {
        title;
        kind = CodeActionKind.quickfix;
        diagnostics = relevant_diagnostics;
        action =
          CodeAction.BothEditThenCommand
            ( WorkspaceEdit.{ changes = UriMap.singleton uri [textEdit] },
              {
                (* https://github.com/microsoft/language-server-protocol/issues/933 *)
                Command.title = "";
                command = Command.Command "log";
                arguments =
                  ["textDocument/codeAction"; "typo"; title]
                  |> List.map (fun str -> Hh_json.JSON_String str);
              } );
      }

let main_of_package ~reader package_dir =
  let json_path = package_dir ^ "/package.json" in
  match Package_heaps.Reader.get_package ~reader json_path with
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
let node_path ~node_resolver_dirnames ~reader ~src_dir require_path =
  let require_path = String_utils.rstrip require_path Files.flow_ext in
  let src_parts = Files.split_path src_dir in
  let req_parts = Files.split_path require_path in
  let string_of_parts parts =
    let str = String.concat "/" parts in
    let str' = String_utils.rstrip str "/index.js" in
    if str == str' then
      String_utils.rstrip str ".js"
    else
      str'
  in
  let (ancestor_rev, to_src, to_req) = find_ancestor_rev src_parts req_parts in
  match to_req with
  | node_modules :: package_dir :: rest when List.mem node_modules node_resolver_dirnames ->
    let package_path =
      package_dir :: node_modules :: ancestor_rev |> Base.List.rev |> String.concat "/"
    in
    (match main_of_package ~reader package_path with
    | Some main when path_matches (String.concat "/" rest) main -> package_dir
    | _ -> string_of_parts (package_dir :: rest))
  | _ ->
    let parts =
      if Base.List.is_empty to_src then
        Filename.current_dir_name :: to_req
      else
        (* add `..` for each dir in `to_src`, to relativize `to_req` *)
        Base.List.fold_left ~f:(fun path _ -> Filename.parent_dir_name :: path) ~init:to_req to_src
    in
    string_of_parts parts

(** [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
    suitable for importing [t] from a file in [src_dir]. that is, if it is a
    filename, returns the path relative to [src_dir]. *)
let path_of_modulename ~node_resolver_dirnames ~reader src_dir = function
  | Modulename.String str -> Some str
  | Modulename.Filename file_key ->
    Base.Option.map
      ~f:(fun src_dir ->
        let path = File_key.to_string file_key in
        node_path ~node_resolver_dirnames ~reader ~src_dir path)
      src_dir

type text_edits = {
  title: string;
  edits: Lsp.TextEdit.t list;
}

let text_edits_of_import ~options ~layout_options ~reader ~src_dir ~ast kind name source =
  let from =
    match source with
    | Export_index.Global -> None
    | Export_index.Builtin from -> Some from
    | Export_index.File_key from ->
      (match Module_heaps.Reader.get_info ~reader ~audit:Expensive.ok from with
      | None -> None
      | Some info ->
        let node_resolver_dirnames = Options.file_options options |> Files.node_resolver_dirnames in
        (match
           path_of_modulename ~node_resolver_dirnames ~reader src_dir info.Module_heaps.module_name
         with
        | None -> None
        | Some from -> Some from))
  in
  match from with
  | None -> None
  | Some from ->
    let title =
      match kind with
      | Export_index.Default -> Printf.sprintf "Import default from %s" from
      | Export_index.Named -> Printf.sprintf "Import from %s" from
      | Export_index.NamedType -> Printf.sprintf "Import type from %s" from
      | Export_index.Namespace -> Printf.sprintf "Import * from %s" from
    in
    let binding = (kind, name) in
    let edits =
      Autofix_imports.add_import ~options:layout_options ~binding ~from ast
      |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
    in
    Some { title; edits }

let suggest_imports ~options ~reader ~ast ~diagnostics ~exports ~name uri loc =
  let open Lsp in
  let files =
    if Autofix_imports.loc_is_type ~ast loc then
      Export_search.get_types name exports
    else
      Export_search.get_values name exports
  in
  if Export_index.ExportSet.is_empty files then
    []
  else
    let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
    let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
    let relevant_diagnostics =
      let open PublishDiagnostics in
      let lsp_code = StringCode Error_codes.(string_of_code CannotResolveName) in
      Base.List.filter diagnostics ~f:(fun { source; code; range; _ } ->
          source = Some "Flow" && code = lsp_code && Lsp_helpers.ranges_overlap range error_range)
    in
    let layout_options =
      Js_layout_generator.{ default_opts with single_quotes = Options.format_single_quotes options }
    in
    Export_index.ExportSet.fold
      (fun (source, export_kind) acc ->
        match
          text_edits_of_import
            ~options
            ~layout_options
            ~reader
            ~src_dir
            ~ast
            export_kind
            name
            source
        with
        | None -> acc
        | Some { edits; title } ->
          let command =
            CodeAction.Action
              {
                CodeAction.title;
                kind = CodeActionKind.quickfix;
                diagnostics = relevant_diagnostics;
                action =
                  CodeAction.BothEditThenCommand
                    ( WorkspaceEdit.{ changes = UriMap.singleton uri edits },
                      {
                        Command.title = "";
                        command = Command.Command "log";
                        arguments =
                          ["textDocument/codeAction"; "import"; title]
                          |> List.map (fun str -> Hh_json.JSON_String str);
                      } );
              }
          in
          command :: acc)
      files
      []

let code_actions_of_errors ~options ~reader ~env ~ast ~diagnostics ~errors uri loc =
  Flow_error.ErrorSet.fold
    (fun error actions ->
      match
        Flow_error.msg_of_error error
        |> Error_message.map_loc_of_error_message (Parsing_heaps.Reader.loc_of_aloc ~reader)
      with
      | Error_message.EEnumInvalidMemberAccess { reason; suggestion = Some suggestion; _ } ->
        let error_loc = Reason.loc_of_reason reason in
        if Loc.intersects error_loc loc then
          let original = reason |> Reason.desc_of_reason |> Reason.string_of_desc in
          create_suggestion ~diagnostics ~original ~suggestion uri error_loc :: actions
        else
          actions
      | Error_message.EBuiltinLookupFailed { reason; name = Some name }
        when Options.autoimports options ->
        let error_loc = Reason.loc_of_reason reason in
        if Loc.intersects error_loc loc then
          let { ServerEnv.exports; _ } = env in
          suggest_imports
            ~options
            ~reader
            ~ast
            ~diagnostics
            ~exports (* TODO consider filtering out internal names *)
            ~name:(Reason.display_string_of_name name)
            uri
            loc
          @ actions
        else
          actions
      | error_message ->
        (match error_message |> Error_message.friendly_message_of_msg with
        | Error_message.PropMissing
            { loc = error_loc; suggestion = Some suggestion; prop = Some prop_name; _ } ->
          if Loc.intersects error_loc loc then
            let original = Printf.sprintf "`%s`" prop_name in
            create_suggestion ~diagnostics ~original ~suggestion uri error_loc :: actions
          else
            actions
        | _ -> actions))
    errors
    []

let code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors =
  Base.List.fold_left
    ~f:(fun acc parse_error ->
      match parse_error with
      | (error_loc, Parse_error.UnexpectedTokenWithSuggestion (token, suggestion)) ->
        if Loc.intersects error_loc loc then
          let original = Printf.sprintf "`%s`" token in
          create_suggestion ~diagnostics ~original ~suggestion uri error_loc :: acc
        else
          acc
      | _ -> acc)
    ~init:[]
    parse_errors

(** currently all of our code actions are quickfixes, so we can short circuit if the client
    doesn't support those. *)
let client_supports_quickfixes params =
  let Lsp.CodeActionRequest.{ context = { only; _ }; _ } = params in
  Lsp.CodeActionKind.contains_kind_opt ~default:true Lsp.CodeActionKind.quickfix only

let code_actions_at_loc
    ~options
    ~env
    ~reader
    ~cx
    ~file_sig
    ~tolerable_errors
    ~ast
    ~typed_ast
    ~parse_errors
    ~diagnostics
    ~uri
    ~loc =
  let experimental_code_actions =
    autofix_exports_code_actions
      ~full_cx:cx
      ~ast
      ~file_sig
      ~tolerable_errors
      ~typed_ast
      ~diagnostics
      uri
      loc
  in
  let error_fixes =
    code_actions_of_errors
      ~options
      ~reader
      ~env
      ~ast
      ~diagnostics
      ~errors:(Context.errors cx)
      uri
      loc
  in
  let parse_error_fixes = code_actions_of_parse_errors ~diagnostics ~uri ~loc parse_errors in
  Lwt.return (Ok (parse_error_fixes @ experimental_code_actions @ error_fixes))

let autofix_exports ~options ~env ~profiling ~file_key ~file_content =
  let open Autofix_exports in
  match%lwt Types_js.typecheck_contents ~options ~env ~profiling file_content file_key with
  | (Some (full_cx, ast, file_sig, tolerable_errors, typed_ast), _, _) ->
    let sv_errors = set_of_fixable_signature_verification_locations tolerable_errors in
    let (new_ast, it_errs) =
      fix_signature_verification_errors ~file_key ~full_cx ~file_sig ~typed_ast ast sv_errors
    in
    Lwt.return (Ok (Insert_type.mk_patch ast new_ast file_content, it_errs))
  | (None, _errs, _) -> Lwt.return (Error "Failed to type-check file")

let insert_type
    ~options
    ~env
    ~profiling
    ~file_key
    ~file_content
    ~target
    ~expand_aliases
    ~omit_targ_defaults
    ~location_is_strict:strict
    ~ambiguity_strategy =
  let open Insert_type in
  match%lwt Types_js.typecheck_contents ~options ~env ~profiling file_content file_key with
  | (Some (full_cx, ast, file_sig, _, typed_ast), _, _) ->
    let result =
      try
        let new_ast =
          Insert_type.insert_type
            ~full_cx
            ~file_sig
            ~typed_ast
            ~expand_aliases
            ~omit_targ_defaults
            ~strict
            ~ambiguity_strategy
            ast
            target
        in
        Ok (mk_patch ast new_ast file_content)
      with FailedToInsertType err -> Error (error_to_string err)
    in
    Lwt.return result
  | (None, errs, _) -> Lwt.return (Error (error_to_string (Expected (FailedToTypeCheck errs))))

module For_tests = struct
  let path_of_modulename = path_of_modulename
end
