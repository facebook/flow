(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
let (>|=) = Lwt.(>|=)

let type_at_pos ~options ~workers ~env ~profiling ~expand_aliases file content line col =
  Types_js.basic_check_contents ~options ~workers ~env ~profiling content file >|=
  function
  | Error str -> Error (str, None)
  | Ok (cx, _info, file_sig) ->
    let loc = Loc.make file line col in
    let json_data, loc, ty =
      let mk_data result_str loc ty_json = Hh_json.JSON_Object [
        "result", Hh_json.JSON_String result_str;
        "loc", Reason.json_of_loc loc;
        "type", ty_json;
      ] in
      Query_types.(
        let type_table = Context.type_table cx in
        let imported_ts = Context.imported_ts cx in
        let file = Context.file cx in
        let result = query_type ~full_cx:cx ~file ~file_sig ~expand_aliases
          ~type_table ~imported_ts loc in
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

let dump_types ~options ~workers ~env ~profiling file content =
  (* Print type using Flow type syntax *)
  let printer = Ty_printer.string_of_t in
  Types_js.basic_check_contents ~options ~workers ~env ~profiling content file >|=
  map ~f:(fun (cx, _info, file_sig) -> Query_types.dump_types cx file_sig ~printer)


let coverage ~options ~workers ~env ~profiling ~force file content =
  let should_check =
    if force then true else
      let (_, docblock) =
        Parsing_service_js.(parse_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  Types_js.basic_check_contents ~options ~workers ~env ~profiling content file >|=
  map ~f:(fun (cx, _, file_sig) -> Query_types.covered_types cx file_sig ~should_check)

let suggest ~options ~workers ~env ~profiling file content =
  Types_js.typecheck_contents ~options ~workers ~env ~profiling content file >|=
  function
  | (Some (cx, ast, file_sig), tc_errors, tc_warnings) ->
    let cxs = Query_types.suggest_types cx file_sig in
    let visitor = new Suggest.visitor ~cxs in
    let typed_ast = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    Ok (tc_errors, tc_warnings, suggest_warnings, typed_ast)
  | (None, errors, _) ->
    Error errors
