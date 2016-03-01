(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Hh_json parsing and pretty printing library.
 *)

type json =
    JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_String of string
  | JSON_Number of string
  | JSON_Bool of bool
  | JSON_Null

exception Syntax_error of string

val json_to_string : json -> string
val json_to_multiline : json -> string
val json_of_string : ?strict:bool -> string -> json
val json_of_file : ?strict:bool -> string -> json

val get_object_exn : json -> (string * json) list
val get_array_exn : json -> json list
val get_string_exn : json -> string
val get_bool_exn : json -> bool

val int_ : int -> json
