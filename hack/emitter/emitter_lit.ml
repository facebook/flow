(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Formatting for literals *)

open Core
open Nast
open Utils

module C = Emitter_core

exception NotLiteral

let rec fmt_lit_exn (_, e) =
  let fmt_str s =
    "s:" ^ string_of_int (String.length s) ^ ":\\\"" ^
      Php_escaping.escape s ^ "\\\";"
  in

  match e with
  | Int (_, s) -> "i:" ^ C.fmt_int s ^ ";"
  | Float (_, x) -> "d:" ^ C.fmt_float x ^ ";"
  | String (_, s) -> fmt_str s
  | Null -> "N;"
  | True -> "b:1;"
  | False -> "b:0;"
  | Array afields ->
    let fmt_afield i = function
      | AFvalue e ->
        i+1,
        "i:" ^ string_of_int i ^ ";" ^ fmt_lit_exn e
      | AFkvalue (ek, ev) ->
        i,
        fmt_lit_exn ek ^ fmt_lit_exn ev
    in
    let _, fields = List.map_env 0 afields fmt_afield in
    "a:" ^ string_of_int (List.length afields) ^
      ":{" ^ String.concat "" fields ^ "}"

  | _ -> raise NotLiteral


let fmt_lit e =
  try
    Some ("\"\"\"" ^ fmt_lit_exn e ^ "\"\"\"")
  with NotLiteral -> None
