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

let to_json input =
  let entries = List.map input begin fun (pos, name, type_) ->
    let line, start, end_ = Pos.info_pos pos in
    Hh_json.JSON_Object [
        "name",  Hh_json.JSON_String name;
        "type", Hh_json.JSON_String type_;
        "line",  Hh_json.int_ line;
        "char_start", Hh_json.int_ start;
        "char_end", Hh_json.int_ end_;
    ]
  end in
  Hh_json.JSON_Array entries

let print_json res =
  print_endline (Hh_json.json_to_string (to_json res));
  ()

let print_readable res =
  List.iter res begin fun (pos, name, type_) ->
    print_endline ((Pos.string pos)^" "^name^" ("^type_^")")
  end;
  ()

let go res output_json =
  if output_json then
    print_json res
  else
    print_readable res
