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

let code_actions_of_errors ~reader ~diagnostics ~errors uri loc =
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

let client_supports_quickfixes only =
  Lsp.CodeActionKind.contains_kind_opt ~default:true Lsp.CodeActionKind.quickfix only

let code_actions_at_loc ~reader ~options ~env ~profiling ~params ~file_key ~file_contents ~loc =
  let open Lsp in
  let CodeActionRequest.{ textDocument; range = _; context = { only; diagnostics } } = params in
  if not (client_supports_quickfixes only) then
    (* currently all of our code actions are quickfixes, so we can short circuit *)
    Lwt.return (Ok [])
  else
    let uri = TextDocumentIdentifier.(textDocument.uri) in
    match%lwt Types_js.typecheck_contents ~options ~env ~profiling file_contents file_key with
    | (Some (full_cx, ast, file_sig, tolerable_errors, typed_ast), _errors, _warnings) ->
      let experimental_code_actions =
        if Inference_utils.well_formed_exports_enabled options file_key then
          autofix_exports_code_actions
            ~full_cx
            ~ast
            ~file_sig
            ~tolerable_errors
            ~typed_ast
            ~diagnostics
            uri
            loc
        else
          []
      in
      let error_fixes =
        code_actions_of_errors ~reader ~diagnostics ~errors:(Context.errors full_cx) uri loc
      in
      Lwt.return (Ok (experimental_code_actions @ error_fixes))
    | (None, _errors, _warnings) -> Lwt.return (Ok [])

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
