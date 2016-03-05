(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
module SS = HackSearchService
module SUtils = SearchUtils

let print_results results =
  List.iter results begin fun res ->
    let pos_string = Pos.string res.SUtils.pos in
    let desc_string =
      ServerSearch.desc_string_from_type res.SUtils.result_type in
    print_endline
      (pos_string^" "^(Utils.strip_ns res.SUtils.name)^", "^desc_string);
  end

let print_results_json results =
  let results =
    Hh_json.JSON_Array (List.map results ServerSearch.result_to_json) in
  print_endline (Hh_json.json_to_string results)

let go results output_json =
  if output_json
  then print_results_json results
  else print_results results
