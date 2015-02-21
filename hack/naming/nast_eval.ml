(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Nast

exception Eval_error of Pos.t

(* This evaluates single-quoted strings the same way PHP does: slashes and
 * single quotes need to be escaped, and all other characters are treated
 * literally -- i.e. '\n' is the literal slash + 'n' *)
let unescape_slashes s =
  let buf = Buffer.create (String.length s) in
  let handle_slash = function
    | '\\'
    | '\'' as c -> Buffer.add_char buf c
    | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c in
  let i = ref 0 in
  while !i < String.length s do
    begin match s.[!i] with
    | '\\' ->
        i := !i + 1;
        handle_slash s.[!i]
    | c ->
        Buffer.add_char buf c
    end;
    i := !i + 1
  done;
  Buffer.contents buf

let rec single_quoted_string = function
  | _, Binop (Ast.Dot, s1, s2) ->
      let p1, s1 = single_quoted_string s1 in
      let _p2, s2 = single_quoted_string s2 in
      p1, s1 ^ s2
  | _, String (p, s) -> p, unescape_slashes s
  | p, _ -> raise (Eval_error p)
