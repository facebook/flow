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
                arguments = [Hh_json.JSON_String title];
              } );
      }

(** [path_of_modulename src_dir t] converts the Modulename.t [t] to a string
    suitable for importing [t] from a file in [src_dir]. that is, if it is a
    filename, returns the path relative to [src_dir]. *)
let path_of_modulename src_dir = function
  | Modulename.String str -> Some str
  | Modulename.Filename f ->
    Base.Option.map
      ~f:(fun src_dir ->
        let rel = Files.relative_path src_dir (File_key.to_string f) in
        if rel.[0] <> '.' then
          "./" ^ rel
        else
          rel)
      src_dir

type text_edits = {
  title: string;
  edits: Lsp.TextEdit.t list;
}

let text_edits_of_import ~options ~reader ~src_dir ~ast kind name from =
  match Module_heaps.Reader.get_info ~reader ~audit:Expensive.ok from with
  | None -> Error ()
  | Some info ->
    (match path_of_modulename src_dir info.Module_heaps.module_name with
    | None -> Error ()
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
        Autofix_imports.add_import ~options ~binding ~from ast
        |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
      in
      Ok { title; edits })

let suggest_imports ~options ~reader ~ast ~diagnostics ~exports ~name uri loc =
  let open Lsp in
  match Export_search.find_opt name exports with
  | None -> []
  | Some files ->
    let src_dir = Lsp_helpers.lsp_uri_to_path uri |> Filename.dirname |> Base.Option.return in
    let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
    let relevant_diagnostics =
      let open PublishDiagnostics in
      diagnostics
      |> List.filter (fun { source; code; range; _ } ->
             source = Some "Flow" && code = StringCode "cannot-resolve-name" && range = error_range)
    in
    let options =
      Js_layout_generator.{ default_opts with single_quotes = Options.format_single_quotes options }
    in
    Export_index.ExportSet.fold
      (fun (file_key, export_kind) acc ->
        match text_edits_of_import ~options ~reader ~src_dir ~ast export_kind name file_key with
        | Error () -> acc
        | Ok { edits; title } ->
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
                        arguments = [Hh_json.JSON_String "import"];
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
        |> Error_message.map_loc_of_error_message (Parsing_heaps_utils.loc_of_aloc ~reader)
      with
      | Error_message.EEnumInvalidMemberAccess { reason; suggestion = Some suggestion; _ } ->
        let error_loc = Reason.loc_of_reason reason in
        if Loc.contains error_loc loc then
          let original = reason |> Reason.desc_of_reason |> Reason.string_of_desc in
          create_suggestion ~diagnostics ~original ~suggestion uri error_loc :: actions
        else
          actions
      | Error_message.EBuiltinLookupFailed { reason; name = Some name } ->
        let error_loc = Reason.loc_of_reason reason in
        if Loc.contains error_loc loc then
          let { ServerEnv.exports; _ } = env in
          suggest_imports ~options ~reader ~ast ~diagnostics ~exports ~name uri loc @ actions
        else
          actions
      | error_message ->
        (match error_message |> Error_message.friendly_message_of_msg with
        | Error_message.PropMissing
            { loc = error_loc; suggestion = Some suggestion; prop = Some prop_name; _ } ->
          if Loc.contains error_loc loc then
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
        if Loc.contains error_loc loc then
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
