(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SS = HackSearchService
module Json = Hh_json
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
  List.iter begin fun res ->
    let pos_string = Pos.string res.SUtils.pos in
    let desc_string = desc_string_from_type res.SUtils.result_type in
    print_endline
      (pos_string^" "^(Utils.strip_ns res.SUtils.name)^", "^desc_string);
  end results

let result_to_json res =
  let desc_string = desc_string_from_type res.SUtils.result_type in
  let scope_string = scope_string_from_type res.SUtils.result_type in
  let p = res.SUtils.pos in
  let fn = Pos.filename p in
  let line, start, end_ = Pos.info_pos p in
  Json.JAssoc [ "name", Json.JString (Utils.strip_ns res.SUtils.name);
                "filename",  Json.JString fn;
                "desc",  Json.JString desc_string;
                "line",  Json.JInt line;
                "char_start", Json.JInt start;
                "char_end", Json.JInt end_;
                "scope", Json.JString scope_string;
              ]

let print_results_json results =
  let results = Json.JList (List.map result_to_json results) in
  print_endline (Json.json_to_string results)

let go results output_json =
  if output_json
  then print_results_json results
  else print_results results
