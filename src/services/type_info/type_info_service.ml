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
    file
    line
    col =
  let loc = Loc.make file line col in
  let (json_data, loc, ty) =
    let mk_data result_str loc ty_json =
      Hh_json.JSON_Object
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
          ~typed_ast
          loc
      in
      match result with
      | FailureNoMatch ->
        (Hh_json.JSON_Object [("result", Hh_json.JSON_String "FAILURE_NO_MATCH")], Loc.none, None)
      | FailureUnparseable (loc, gt, _) ->
        let json = Hh_json.JSON_String (Type.string_of_ctor gt) in
        (mk_data "FAILURE_UNPARSEABLE" loc json, loc, None)
      | Success (loc, ty) ->
        (* TODO use Ty_debug.json_of_t after making it faster using
             count_calls *)
        let json = Hh_json.JSON_String (Ty_printer.string_of_t ty) in
        (mk_data "SUCCESS" loc json, loc, Some ty))
  in
  ((loc, ty), Some json_data)

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

let dump_types ~options ~env ~profiling ~expand_aliases ~evaluate_type_destructors file content =
  (* Print type using Flow type syntax *)
  let printer = Ty_printer.string_of_t in
  Types_js.type_contents ~options ~env ~profiling content file
  >|= map ~f:(fun (cx, _info, file_sig, tast, _parse_errors) ->
          let abs_file_sig = File_sig.abstractify_locs file_sig in
          Query_types.dump_types
            ~printer
            ~expand_aliases
            ~evaluate_type_destructors
            cx
            abs_file_sig
            tast)

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

let suggest ~options ~env ~profiling file_name file_content =
  let file_key = File_key.SourceFile file_name in
  Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
  | (Some (cx, ast, file_sig, tast), tc_errors, tc_warnings) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let ty_query = Query_types.suggest_types cx file_sig tast in
    let visitor = new Suggest.visitor ~ty_query in
    let ast_with_suggestions = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    let ast_diff = Flow_ast_differ.(program Standard ast ast_with_suggestions) in
    let file_patch =
      Replacement_printer.mk_patch_ast_differ ast_diff ast_with_suggestions file_content
    in
    Ok (tc_errors, tc_warnings, suggest_warnings, file_patch)
  | (None, errors, _) -> Error errors

let code_actions_at_loc ~options ~env ~profiling ~params ~file_key ~file_contents ~loc =
  Lsp.(
    CodeAction.(
      CodeActionRequest.(
        CodeActionKind.(
          let { textDocument; range = _; context } = params in
          let uri = TextDocumentIdentifier.(textDocument.uri) |> Lsp.string_of_uri in
          Types_js.typecheck_contents ~options ~env ~profiling file_contents file_key >|= function
          | (Some (full_cx, ast, file_sig, typed_ast), _, _) ->
            Autofix_exports.(
              let fixable_locs = set_of_fixable_signature_verification_locations file_sig in
              if
                contains_kind_opt ~default:true quickfix context.only && LocSet.mem loc fixable_locs
              then
                match
                  fix_signature_verification_error_at_loc ~full_cx ~file_sig ~typed_ast ast loc
                with
                | new_ast ->
                  let diff = Insert_type.mk_diff ast new_ast in
                  let edits =
                    Replacement_printer.mk_loc_patch_ast_differ diff ast
                    |> Flow_lsp_conversions.flow_loc_patch_to_lsp_edits
                  in
                  Ok
                    [
                      Action
                        {
                          CodeAction.title = "insert type annotation";
                          kind = quickfix;
                          (* Handing back the diagnostics we were given is a placeholder for
               eventually generating the diagnostics for the errors we are fixing *)
                          diagnostics = CodeActionRequest.(context.diagnostics);
                          action = EditOnly WorkspaceEdit.{ changes = SMap.of_list [(uri, edits)] };
                        };
                    ]
              else
                Ok [])
          | _ -> Ok []))))
