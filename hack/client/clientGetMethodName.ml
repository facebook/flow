(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_json

let get_result_type res =
  match res.IdentifySymbolService.type_ with
  | IdentifySymbolService.Class -> "class"
  | IdentifySymbolService.Method _ -> "method"
  | IdentifySymbolService.Function -> "function"
  | IdentifySymbolService.LocalVar -> "local"
  | IdentifySymbolService.Property _ -> "property"
  | IdentifySymbolService.ClassConst _ -> "class_const"

let to_json = function
  | Some res ->
    JSON_Object [
      "name",           JSON_String res.IdentifySymbolService.name;
      "result_type",    JSON_String (get_result_type res);
      "pos",            Pos.json (res.IdentifySymbolService.pos);
      "internal_error", JSON_Bool false;
    ]
  | None -> JSON_Object [ "internal_error", JSON_Bool false ]

let print_json res =
  print_endline (Hh_json.json_to_string (to_json res))

let print_readable = function
  | Some res ->
    let line, start, end_ = Pos.info_pos res.IdentifySymbolService.pos in
    Printf.printf "Name: %s, type: %s, position: line %d, characters %d-%d\n"
      res.IdentifySymbolService.name
      (get_result_type res)
      line start end_
  | None -> ()

let go res output_json =
  if output_json then
    print_json res
  else
    print_readable res
