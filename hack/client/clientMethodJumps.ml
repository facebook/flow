(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Json = Hh_json

let pos_to_json pos =
  let line, start, end_ = Pos.info_pos pos in
  Json.JAssoc [
    "file", Json.JString pos.Pos.pos_file;  (* we can't use Pos.json *)
    "line", Json.JInt line;
    "char_start", Json.JInt start;
    "char_end", Json.JInt end_;
  ]

let cls_or_mthd_to_json name pos p_name =
  Json.JAssoc [
    "name", Json.JString (Utils.strip_ns name);
    "pos", pos_to_json pos;
    "parent_name", Json.JString (Utils.strip_ns p_name);
  ]

let to_json input =
  let entries = List.map begin fun res ->
    Json.JAssoc [
      "origin", cls_or_mthd_to_json
        res.MethodJumps.orig_name
        res.MethodJumps.orig_pos
        res.MethodJumps.orig_p_name;
      "destination", cls_or_mthd_to_json
        res.MethodJumps.dest_name
        res.MethodJumps.dest_pos
        res.MethodJumps.dest_p_name;
    ]
    end input in
  Json.JList entries

let print_json res =
  print_endline (Json.json_to_string (to_json res));
  ()

let readable_place name pos p_name =
  let readable = (Pos.string pos) in
  if String.length p_name <> 0
  then (readable^" "^(Utils.strip_ns p_name)^"::"^(Utils.strip_ns name))
  else (readable^" "^(Utils.strip_ns name))

let print_readable res find_children =
  List.iter begin fun res ->
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
  end res;
  ()

let go res find_children output_json =
  if output_json then
    print_json res
  else
    print_readable res find_children
