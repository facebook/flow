(**
 * Copyright (c) 2015, Facebook, Inc.
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
    print_endline (Hh_json.json_to_string (ServerInferType.to_json pos ty))
  end else begin
    match ty with
      | Some ty -> print_endline ty
      | None -> print_endline "(unknown)"
  end
