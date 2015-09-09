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
module Json = Hh_json

let to_json input =
  let entries = List.map input begin fun (pos, name, type_) ->
    let line, start, end_ = Pos.info_pos pos in
    Json.JAssoc [ "name",  Json.JString name;
                  "type", Json.JString type_;
                  "line",  Json.JInt line;
                  "char_start", Json.JInt start;
                  "char_end",Json.JInt end_;
                ]
  end in
  Json.JList entries

let print_json res =
  print_endline (Json.json_to_string (to_json res));
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
