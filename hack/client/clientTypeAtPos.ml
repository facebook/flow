(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go pos ty output_json =
  if output_json
  then begin
    let ty_json = match ty with
      | Some ty -> Hh_json.JString ty
      | None -> Hh_json.JNull
    in
    let pos_json = match pos with
      | Some pos -> Pos.json pos
      | None -> Hh_json.JNull
    in
    print_endline (Hh_json.json_to_string
                    (Hh_json.JAssoc [
                      "type", ty_json;
                      "pos", pos_json;
                    ]))
  end else begin
    match ty with
      | Some ty -> print_endline ty
      | None -> print_endline "(unknown)"
  end
