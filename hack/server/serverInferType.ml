(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = Pos.absolute option * string option

let go (env:ServerEnv.env) (fn, line, char) =
  let get_result = InferAtPosService.attach_hooks line char in
  ignore @@ ServerIdeUtils.check_file_input
    env.ServerEnv.tcopt env.ServerEnv.files_info fn;
  let pos, ty = get_result () in
  let pos = Option.map pos Pos.to_absolute in
  InferAtPosService.detach_hooks ();
  pos, ty

let to_json pos ty =
  let ty_json = match ty with
    | Some ty -> Hh_json.JSON_String ty
    | None -> Hh_json.JSON_Null
  in
  let pos_json = match pos with
    | Some pos -> Pos.json pos
    | None -> Hh_json.JSON_Null
  in
  Hh_json.JSON_Object [
    "type", ty_json;
    "pos", pos_json;
  ]
