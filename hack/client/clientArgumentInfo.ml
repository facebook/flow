(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go results output_json =
  let results_json = Hh_json.JSON_Object (ArgumentInfoService.to_json results) in
  print_endline (Hh_json.json_to_string results_json)
