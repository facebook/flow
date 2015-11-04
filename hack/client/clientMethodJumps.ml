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

let pos_to_json pos =
  let line, start, end_ = Pos.info_pos pos in
  Hh_json.JSON_Object [
    "file", Hh_json.JSON_String (Pos.filename pos);  (* we can't use Pos.json *)
    "line", Hh_json.int_ line;
    "char_start", Hh_json.int_ start;
    "char_end", Hh_json.int_ end_;
  ]

let cls_or_mthd_to_json name pos p_name =
  Hh_json.JSON_Object [
    "name", Hh_json.JSON_String (Utils.strip_ns name);
    "pos", pos_to_json pos;
    "parent_name", Hh_json.JSON_String (Utils.strip_ns p_name);
  ]

let to_json input =
  let entries = List.map input begin fun res ->
    Hh_json.JSON_Object [
      "origin", cls_or_mthd_to_json
        res.MethodJumps.orig_name
        res.MethodJumps.orig_pos
        res.MethodJumps.orig_p_name;
      "destination", cls_or_mthd_to_json
        res.MethodJumps.dest_name
        res.MethodJumps.dest_pos
        res.MethodJumps.dest_p_name;
    ]
  end in
  Hh_json.JSON_Array entries

let print_json res =
  print_endline (Hh_json.json_to_string (to_json res));
  ()

let readable_place name pos p_name =
  let readable = (Pos.string pos) in
  if String.length p_name <> 0
  then (readable^" "^(Utils.strip_ns p_name)^"::"^(Utils.strip_ns name))
  else (readable^" "^(Utils.strip_ns name))

let print_readable res find_children =
  List.iter res begin fun res ->
    let origin_readable = readable_place
        res.MethodJumps.orig_name
        res.MethodJumps.orig_pos
        res.MethodJumps.orig_p_name in
    let dest_readable = readable_place
        res.MethodJumps.dest_name
        res.MethodJumps.dest_pos
        res.MethodJumps.dest_p_name in
    let extended = "inherited "^(if find_children then "by" else "from") in
    print_endline (origin_readable^"\n    "^extended^" "^dest_readable);
  end;
  ()

let go res find_children output_json =
  if output_json then
    print_json res
  else
    print_readable res find_children
