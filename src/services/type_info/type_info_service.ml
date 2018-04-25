(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result

let type_at_pos ~options ~workers ~env ~profiling file content line col =
  let%lwt result = Types_js.basic_check_contents ~options ~workers ~env ~profiling content file in
  map_error ~f:(fun str -> str, None) result
  >>| (fun (cx, _info) ->
    let loc = Loc.make file line col in
    let json_data, loc, ty =
      let mk_data result_str loc ty_json = Hh_json.JSON_Object [
        "result", Hh_json.JSON_String result_str;
        "loc", Reason.json_of_loc loc;
        "type", ty_json;
      ] in
      Query_types.(match query_type cx loc with
        | FailureNoMatch ->
          Hh_json.JSON_Object ["result", Hh_json.JSON_String "FAILURE_NO_MATCH"], Loc.none, None
        | FailureUnparseable (loc, gt, _) ->
          mk_data "FAILURE_UNPARSEABLE" loc (Hh_json.JSON_String (Type.string_of_ctor gt)), loc, None
        | Success (loc, ty) ->
          (* TODO use Ty_debug.json_of_t after making it faster using
             count_calls *)
          mk_data "SUCCESS" loc (Hh_json.JSON_String (Ty_printer.string_of_t ty)),
            loc, Some ty
      )
    in
    let ty = Option.map ~f:Ty_printer.string_of_t ty in
    (loc, ty), Some json_data
  ) |> Lwt.return

let dump_types ~options ~workers ~env ~profiling file content =
  (* Print type using Flow type syntax *)
  let printer = Ty_printer.string_of_t in

  let%lwt result = Types_js.basic_check_contents ~options ~workers ~env ~profiling content file in
  result
  >>| (fun (cx, _info) -> Query_types.dump_types ~printer cx)
  |> Lwt.return

let coverage ~options ~workers ~env ~profiling ~force file content =
  let should_check =
    if force then
      true
    else
      let (_, docblock) =
        Parsing_service_js.(parse_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  let%lwt result = Types_js.basic_check_contents ~options ~workers ~env ~profiling content file in
  result
  >>| (fun (cx, _) -> Query_types.covered_types cx ~should_check)
  |> Lwt.return

let suggest ~options ~workers ~env ~profiling file content =
  let%lwt result = Types_js.typecheck_contents ~options ~workers ~env ~profiling content file in
  Lwt.return @@
  match result with
  | (Some (cx, ast), tc_errors, tc_warnings) ->
    let cxs = Query_types.suggest_types cx in
    let visitor = new Suggest.visitor ~cxs in
    let typed_ast = visitor#program ast in
    let suggest_warnings = visitor#warnings () in
    Ok (tc_errors, tc_warnings, suggest_warnings, typed_ast)
  | (None, errors, _) ->
    Error errors
