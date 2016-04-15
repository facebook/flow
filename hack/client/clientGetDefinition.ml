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

let to_json = function
  | Some res ->
    let definition_pos = match res.IdentifySymbolService.name_pos with
      | Some pos -> Pos.json pos
      | None -> JSON_Null
    in
    JSON_Object [
      "name",           JSON_String res.IdentifySymbolService.name;
      "result_type",    JSON_String (ClientGetMethodName.get_result_type res);
      "pos",            Pos.json (res.IdentifySymbolService.pos);
      "definition_pos", definition_pos;
    ]
  | None -> JSON_Null

let print_json res =
  print_endline (Hh_json.json_to_string (to_json res))

let print_readable = function
  | Some res ->
    Printf.printf "Name: %s, type: %s, position: %s"
      res.IdentifySymbolService.name
      (ClientGetMethodName.get_result_type res)
      (Pos.string_no_file res.IdentifySymbolService.pos);
    Option.iter res.IdentifySymbolService.name_pos begin fun pos ->
      Printf.printf ", defined: %s" (Pos.string_no_file pos)
    end;
    print_newline ()
  | None -> ()

let go res output_json =
  if output_json then
    print_json res
  else
    print_readable res
