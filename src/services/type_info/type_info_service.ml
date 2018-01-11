(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result

let mk_loc file line col =
  {
    Loc.
    source = Some file;
    start = { Loc.line; column = col; offset = 0; };
    _end = { Loc.line; column = col + 1; offset = 0; };
  }

let type_at_pos ~options ~workers ~env ~profiling file content line col =
  Types_js.basic_check_contents ~options ~workers ~env ~profiling content file
  |> map_error ~f:(fun str -> str, None)
  >>| fun (cx, _info) ->
    let loc = mk_loc file line col in
    let json_data, loc, ground_t, possible_ts =
      let mk_data result_str loc ty = Hh_json.JSON_Object [
        "result", Hh_json.JSON_String result_str;
        "loc", Reason.json_of_loc loc;
        "type", Debug_js.json_of_t ~depth:3 cx ty;
      ] in
      Query_types.(match query_type cx loc with
        | FailureNoMatch ->
          Hh_json.JSON_Object ["result", Hh_json.JSON_String "FAILURE_NO_MATCH"], Loc.none, None, []
        | FailureUnparseable (loc, gt, possible_ts) ->
          mk_data "FAILURE_UNPARSEABLE" loc gt, loc, None, possible_ts
        | Success (loc, gt, possible_ts) ->
          mk_data "SUCCESS" loc gt, loc, Some gt, possible_ts
      )
    in
    let ty = match ground_t with
      | None -> None
      | Some t -> Some (Type_printer.string_of_t cx t)
    in
    let reasons = List.map Type.reason_of_t possible_ts in
    (loc, ty, reasons), Some json_data

let dump_types ~options ~workers ~env file content =
  (* Print type using Flow type syntax *)
  let printer = Type_printer.string_of_t in

  Types_js.deprecated_basic_check_contents ~options ~workers ~env content file
  >>| fun (_profiling, cx, _info) ->
    Query_types.dump_types printer cx

let coverage ~options ~workers ~env ~force file content =
  let should_check =
    if force then
      true
    else
      let (_, docblock) =
        Parsing_service_js.(get_docblock docblock_max_tokens file content) in
      Docblock.is_flow docblock
  in
  Types_js.deprecated_basic_check_contents ~options ~workers ~env content file
  >>| fun (_profiling, cx, _info) ->
    let types = Query_types.covered_types cx in
    if should_check then
      types
    else
      types |> List.map (fun (loc, _) -> (loc, false))


let suggest ~options ~workers ~env file region content =
  Types_js.deprecated_basic_check_contents ~options ~workers ~env content file
  >>| fun (_profiling, cx, _info) ->
    Query_types.fill_types cx
    |> List.sort Pervasives.compare
    |> match region with
      | [] -> fun insertions -> insertions
      | [l1;c1;l2;c2] ->
          let l1,c1,l2,c2 =
            int_of_string l1,
            int_of_string c1,
            int_of_string l2,
            int_of_string c2
          in
          List.filter (fun (l,c,_) ->
            (l1,c1) <= (l,c) && (l,c) <= (l2,c2)
          )
      | _ -> assert false

