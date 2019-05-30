(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
let (>|=) = Lwt.(>|=)

let type_at_pos ~options ~env ~profiling ~expand_aliases ~omit_targ_defaults file content line col =
  Types_js.basic_check_contents ~options ~env ~profiling content file >|=
  function
  | Error str -> Error (str, None)
  | Ok (cx, _info, file_sig, typed_ast) ->
    let loc = Loc.make file line col in
    let json_data, loc, ty =
      let mk_data result_str loc ty_json = Hh_json.JSON_Object [
        "result", Hh_json.JSON_String result_str;
        "loc", Reason.json_of_loc ~offset_table:None loc;
        "type", ty_json;
      ] in
      Query_types.(
        let file = Context.file cx in
        let result = type_at_pos_type
          ~full_cx:cx
          ~file
          ~file_sig:(File_sig.abstractify_locs file_sig)
          ~expand_aliases
          ~omit_targ_defaults
          ~typed_ast
          loc
        in
        match result with
        | FailureNoMatch ->
          Hh_json.JSON_Object [
            "result", Hh_json.JSON_String "FAILURE_NO_MATCH"
          ], Loc.none, None
        | FailureUnparseable (loc, gt, _) ->
          let json = Hh_json.JSON_String (Type.string_of_ctor gt) in
          mk_data "FAILURE_UNPARSEABLE" loc json, loc, None
        | Success (loc, ty) ->
          (* TODO use Ty_debug.json_of_t after making it faster using
             count_calls *)
          let json = Hh_json.JSON_String (Ty_printer.string_of_t ty) in
          mk_data "SUCCESS" loc json, loc, Some ty
      ) in

    Ok ((loc, ty), Some json_data)

let dump_types ~options ~env ~profiling file content =
  (* Print type using Flow type syntax *)
  let printer = Ty_printer.string_of_t in
  Types_js.basic_check_contents ~options ~env ~profiling content file >|=
  map ~f:(fun (cx, _info, file_sig, tast) ->
    let abs_file_sig = File_sig.abstractify_locs file_sig in
    Query_types.dump_types ~printer cx abs_file_sig tast
  )


let coverage ~options ~env ~profiling ~force ~trust file content =
  let should_check =
    if force then true else
      let (_, docblock) =
        Parsing_service_js.(parse_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  Types_js.basic_check_contents ~options ~env ~profiling content file >|=
  map ~f:(fun (cx, _, _, _) -> Query_types.covered_types cx ~should_check ~check_trust:trust)


let suggest ~options ~env ~profiling file_name file_content =
  let file_key = File_key.SourceFile file_name in
  Types_js.typecheck_contents ~options ~env ~profiling file_content file_key >|= function
  | (Some (cx, ast, file_sig, tast), tc_errors, tc_warnings) ->
    let file_sig = File_sig.abstractify_locs file_sig in
    let ty_query loc = Query_types.suggest_types cx file_sig tast (ALoc.of_loc loc) in
    let visitor = new Suggest.visitor ~ty_query in
    let ast_with_suggestions = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    let ast_diff = Flow_ast_differ.(program Standard ast ast_with_suggestions) in
    let file_patch =
      Replacement_printer.mk_patch_ast_differ ast_diff ast_with_suggestions file_content
    in
    Ok (tc_errors, tc_warnings, suggest_warnings, file_patch)
  | (None, errors, _) ->
    Error errors
