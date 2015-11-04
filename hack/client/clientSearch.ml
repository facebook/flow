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

let desc_string_from_type result_type =
  match result_type with
    | SS.Class class_kind ->
        (match class_kind with
          | Ast.Cabstract -> "abstract class"
          | Ast.Cnormal -> "class"
          | Ast.Cinterface -> "interface"
          | Ast.Ctrait -> "trait"
          | Ast.Cenum -> "enum")
    | SS.Method (static, scope) ->
        if static
        then "static method in "^scope
        else "method in "^scope
    | SS.ClassVar (static, scope) ->
        if static
        then "static class var in "^scope
        else "class var in "^scope
    | SS.Function -> "function"
    | SS.Typedef -> "typedef"
    | SS.Constant -> "constant"

let scope_string_from_type result_type =
  match result_type with
    | SS.Method (_, scope)
    | SS.ClassVar (_, scope) -> scope
    | _ -> ""

let print_results results =
  List.iter results begin fun res ->
    let pos_string = Pos.string res.SUtils.pos in
    let desc_string = desc_string_from_type res.SUtils.result_type in
    print_endline
      (pos_string^" "^(Utils.strip_ns res.SUtils.name)^", "^desc_string);
  end

let result_to_json res =
  let desc_string = desc_string_from_type res.SUtils.result_type in
  let scope_string = scope_string_from_type res.SUtils.result_type in
  let p = res.SUtils.pos in
  let fn = Pos.filename p in
  let line, start, end_ = Pos.info_pos p in
  Hh_json.JSON_Object [ "name", Hh_json.JSON_String (Utils.strip_ns res.SUtils.name);
                "filename",  Hh_json.JSON_String fn;
                "desc",  Hh_json.JSON_String desc_string;
                "line",  Hh_json.int_ line;
                "char_start", Hh_json.int_ start;
                "char_end", Hh_json.int_ end_;
                "scope", Hh_json.JSON_String scope_string;
              ]

let print_results_json results =
  let results = Hh_json.JSON_Array (List.map results result_to_json) in
  print_endline (Hh_json.json_to_string results)

let go results output_json =
  if output_json
  then print_results_json results
  else print_results results
