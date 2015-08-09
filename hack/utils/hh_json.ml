(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Quick and dirty Json pretty printing library.
 *
 *)

type json =
    JList of json list
  | JBool of bool
  | JString of string
  | JAssoc of (string * json) list
  | JNull
  | JInt of int
  | JFloat of float

let rec json_to_string (json:json): string =
  match json with
    JList l ->
      let nl = List.map json_to_string l in
      "[" ^ (String.concat "," nl) ^ "]"
  | JAssoc l ->
      let nl = List.map (fun (k, v) -> (json_to_string (JString k)) ^ ":" ^ (json_to_string v)) l in
      "{" ^ (String.concat "," nl) ^ "}"
  | JBool b -> if b then "true" else "false"
  | JString s -> Printf.sprintf "%S" s
  | JNull -> "null"
  | JInt i -> Printf.sprintf "%d" i
  | JFloat f -> Printf.sprintf "%f" f
