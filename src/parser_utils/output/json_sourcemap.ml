(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Translator from Hh_json to Sourcemap's abstract JSON representation *)
module Json_translator = struct
  type t = Hh_json.json

  let of_string x = Hh_json.JSON_String x

  let of_obj props = Hh_json.JSON_Object props

  let of_array arr = Hh_json.JSON_Array arr

  let of_number x = Hh_json.JSON_Number x

  let null = Hh_json.JSON_Null

  let to_string t =
    match t with
    | Hh_json.JSON_String x -> x
    | _ -> raise (Hh_json.Syntax_error "expected string")

  let to_obj t =
    match t with
    | Hh_json.JSON_Object x -> x
    | _ -> raise (Hh_json.Syntax_error "expected object")

  let to_array t =
    match t with
    | Hh_json.JSON_Array x -> x
    | _ -> raise (Hh_json.Syntax_error "expected array")

  let to_number t =
    match t with
    | Hh_json.JSON_Number x -> x
    | _ -> raise (Hh_json.Syntax_error "expected number")

  let is_null t = t = Hh_json.JSON_Null
end

module Writer = Sourcemap.Make_json_writer (Json_translator)
module Reader = Sourcemap.Make_json_reader (Json_translator)

let json_of_sourcemap map = Writer.json_of_sourcemap map

let sourcemap_of_json json = Reader.sourcemap_of_json json

let sourcemap_of_string str = sourcemap_of_json (Hh_json.json_of_string str)
