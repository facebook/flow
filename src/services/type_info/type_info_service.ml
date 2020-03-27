(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result

let ( >|= ) = Lwt.( >|= )

let type_at_pos
    ~cx
    ~file_sig
    ~typed_ast
    ~expand_aliases
    ~omit_targ_defaults
    ~evaluate_type_destructors
    ~max_depth
    ~verbose_normalizer
    file
    line
    col =
  let loc = Loc.make file line col in
  let (json_data, loc, ty) =
    let mk_data result_str loc ty_json =
      [
        ("result", Hh_json.JSON_String result_str);
        ("loc", Reason.json_of_loc ~offset_table:None loc);
        ("type", ty_json);
      ]
    in
    Query_types.(
      let file = Context.file cx in
      let result =
        type_at_pos_type
          ~full_cx:cx
          ~file
          ~file_sig:(File_sig.abstractify_locs file_sig)
          ~expand_aliases
          ~omit_targ_defaults
          ~evaluate_type_destructors
          ~verbose_normalizer
          ~max_depth
          ~typed_ast
          loc
      in
      match result with
      | FailureNoMatch -> ([("result", Hh_json.JSON_String "FAILURE_NO_MATCH")], Loc.none, None)
      | FailureUnparseable (loc, gt, _) ->
        let json = Hh_json.JSON_String (Type.string_of_ctor gt) in
        (mk_data "FAILURE_UNPARSEABLE" loc json, loc, None)
      | Success (loc, ty) ->
        (* TODO use Ty_debug.json_of_t after making it faster using
             count_calls *)
        let json = Hh_json.JSON_String (Ty_printer.string_of_elt ty) in
        (mk_data "SUCCESS" loc json, loc, Some ty))
  in
  ((loc, ty), json_data)

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
  Insert_type.(
    Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
    | (Some (full_cx, ast, file_sig, typed_ast), _, _) ->
      begin
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
      end
    | (None, errs, _) -> Error (error_to_string (Expected (FailedToTypeCheck errs))))

let autofix_exports ~options ~env ~profiling ~file_key ~file_content =
  Autofix_exports.(
    Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
    | (Some (full_cx, ast, file_sig, typed_ast), _, _) ->
      let sv_errors = set_of_fixable_signature_verification_locations file_sig in
      let fix_sv_errors = fix_signature_verification_errors ~full_cx ~file_sig ~typed_ast in
      let (new_ast, it_errs) = fix_sv_errors ast sv_errors in
      Ok (Insert_type.mk_patch ast new_ast file_content, it_errs)
    | (None, _errs, _) -> Error ":o")

let dump_types ~expand_aliases ~evaluate_type_destructors cx file_sig typed_ast =
  (* Print type using Flow type syntax *)
  let printer = Ty_printer.string_of_elt_single_line in
  let abs_file_sig = File_sig.abstractify_locs file_sig in
  Query_types.dump_types
    ~printer
    ~expand_aliases
    ~evaluate_type_destructors
    cx
    abs_file_sig
    typed_ast

let coverage ~cx ~typed_ast ~force ~trust file content =
  let should_check =
    if force then
      true
    else
      (* We can't just use the docblock that type_contents returns because type_contents modifies
      * it and we want the original docblock. Fortunately this is a pure function, and pretty fast,
      * so recomputing it isn't a problem. *)
      let (_, docblock) = Parsing_service_js.(parse_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  Coverage.covered_types cx ~should_check ~check_trust:trust typed_ast

let suggest ~options ~env ~profiling file_key file_content =
  Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
  | (Some (cx, ast, file_sig, tast), tc_errors, tc_warnings) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let ty_query = Query_types.suggest_types cx file_sig tast in
    let visitor = new Suggest.visitor ~ty_query in
    let ast_with_suggestions = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    let ast_diff = Flow_ast_differ.(program Standard ast ast_with_suggestions) in
    let file_patch = Replacement_printer.mk_patch_ast_differ ast_diff file_content in
    Ok (tc_errors, tc_warnings, suggest_warnings, file_patch)
  | (None, errors, _) -> Error errors

let code_actions_at_loc ~reader ~options ~env ~profiling ~params ~file_key ~file_contents ~loc =
  let open Lsp in
  let CodeActionRequest.{ textDocument; range = _; context } = params in
  let uri = TextDocumentIdentifier.(textDocument.uri) |> string_of_uri in
  let autofix_exports_code_actions ~full_cx ~ast ~file_sig ~typed_ast =
    let open Autofix_exports in
    let fixable_locs = set_of_fixable_signature_verification_locations file_sig in
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
              diagnostics = context.CodeActionRequest.diagnostics;
              action = CodeAction.EditOnly WorkspaceEdit.{ changes = SMap.of_list [(uri, edits)] };
            };
        ]
    else
      []
  in
  let create_suggestion loc ~original ~suggestion =
    let title = Printf.sprintf "Replace %s with `%s`" original suggestion in
    let error_range = Flow_lsp_conversions.loc_to_lsp_range loc in
    let relevant_diagnostics =
      context.CodeActionRequest.diagnostics
      |> List.filter (fun PublishDiagnostics.{ range; _ } -> range = error_range)
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
              ( WorkspaceEdit.{ changes = SMap.singleton uri [textEdit] },
                {
                  (* https://github.com/microsoft/language-server-protocol/issues/933 *)
                  Command.title = "";
                  command = Command.Command "log";
                  arguments = [Hh_json.JSON_String title];
                } );
        }
  in
  let code_actions_of_errors errors =
    Flow_error.ErrorSet.fold
      (fun error actions ->
        match
          Flow_error.msg_of_error error
          |> Error_message.map_loc_of_error_message (Parsing_heaps_utils.loc_of_aloc ~reader)
        with
        | Error_message.EEnumInvalidMemberAccess { reason; suggestion = Some suggestion; _ } ->
          let loc = Reason.loc_of_reason reason in
          let original = reason |> Reason.desc_of_reason |> Reason.string_of_desc in
          create_suggestion loc ~original ~suggestion :: actions
        | error_message ->
          (match error_message |> Error_message.friendly_message_of_msg with
          | Error_message.PropMissing
              { loc; suggestion = Some suggestion; prop = Some prop_name; _ } ->
            let original = Printf.sprintf "`%s`" prop_name in
            create_suggestion loc ~original ~suggestion :: actions
          | _ -> actions))
      errors
      []
  in
  Types_js.typecheck_contents ~options ~env ~profiling file_contents file_key >|= function
  | (Some (full_cx, ast, file_sig, typed_ast), _, _)
    when CodeActionKind.contains_kind_opt
           ~default:true
           CodeActionKind.quickfix
           context.CodeActionRequest.only ->
    (* The current ide-lsp-server/flow-lsp-client doesn't necessarily get restarted for every project.
     * Checking the option here ensures that the flow server doesn't do too much work for code
     * action requests on projects where code actions are not enabled in the `.flowconfig`. *)
    let experimental_code_actions =
      if options.Options.opt_autofix_exports then
        autofix_exports_code_actions ~full_cx ~ast ~file_sig ~typed_ast
      else
        []
    in
    let code_actions =
      experimental_code_actions @ code_actions_of_errors (Context.errors full_cx)
    in
    Ok code_actions
  | _ -> Ok []
