(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let mk_loc file line col =
  {
    Loc.
    source = Some file;
    start = { Loc.line; column = col; offset = 0; };
    _end = { Loc.line; column = col + 1; offset = 0; };
  }


let type_at_pos ~options ~client_context ~verbose ~include_raw
    file content line col =
  let profiling, cx =
    match Types_js.typecheck_contents ?verbose ~options content file with
    | profiling, Some cx, _, _ -> profiling, cx
    | _  -> failwith "Couldn't parse file"
  in
  let loc = mk_loc file line col in
  let result_str, data, loc, ground_t, possible_ts =
    let mk_data loc ty = [
      "loc", Reason.json_of_loc loc;
      "type", Debug_js.json_of_t ~depth:3 cx ty
    ] in
    Query_types.(match query_type cx loc with
    | FailureNoMatch ->
      "FAILURE_NO_MATCH", [],
      Loc.none, None, []
    | FailureUnparseable (loc, gt, possible_ts) ->
      "FAILURE_UNPARSEABLE", mk_data loc gt,
      loc, None, possible_ts
    | Success (loc, gt, possible_ts) ->
      "SUCCESS", mk_data loc gt,
      loc, Some gt, possible_ts
  ) in
  FlowEventLogger.type_at_pos_result
    ~client_context
    ~result_str
    ~json_data:(Hh_json.JSON_Object data)
    ~profiling;

  let ty, raw_type = match ground_t with
    | None -> None, None
    | Some t ->
        let ty = Some (Type_printer.string_of_t cx t) in
        let raw_type =
          if include_raw then
            Some (Debug_js.jstr_of_t ~size:50 ~depth:10 cx t)
          else
            None
        in
        ty, raw_type
  in
  let reasons =
    possible_ts
    |> List.map Type.reason_of_t
  in
  (loc, ty, raw_type, reasons)


let dump_types ~options ~include_raw ~strip_root file content =
  (* Print type using Flow type syntax *)
  let printer = Type_printer.string_of_t in

  (* Print raw representation of types as json; as it turns out, the
     json gets cut off at a specified depth, so pass the maximum
     possible depth to avoid that. *)
  let raw_printer c t =
    if include_raw
      then Some (Debug_js.jstr_of_t ~depth:max_int ~strip_root c t)
      else None
    in

  let cx = match Types_js.typecheck_contents ~options content file with
  | _, Some cx, _, _ -> cx
  | _  -> failwith "Couldn't parse file" in

  Query_types.dump_types printer raw_printer cx


let coverage ~options ~force file content =
  let should_check =
    if force then
      true
    else
      let (_, docblock) =
        Parsing_service_js.get_docblock Docblock.max_tokens file content in
      Docblock.is_flow docblock
  in
  let cx = match Types_js.typecheck_contents ~options content file with
  | _, Some cx, _, _ -> cx
  | _  -> failwith "Couldn't parse file" in
  let types = Query_types.covered_types cx in
  if should_check then
    types
  else
    types |> List.map (fun (loc, _) -> (loc, false))
