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

let print_result (name, pos) =
  let pos_str = Pos.string pos in
  print_endline (pos_str ^ " " ^ name);
  ()

let print_json res =
  print_endline (Hh_json.json_to_string (ServerFindRefs.to_json res))

let print_readable res =
  List.iter res print_result;
  print_endline ((string_of_int (List.length res)) ^ " total results")

let go (res : FindRefsService.result) output_json =
  if output_json then
    print_json res
  else
    print_readable res
