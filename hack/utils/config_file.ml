(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(* OCaml 4.0.0 defines this for us, but we still need to support 3.12 *)
let trim s =
  let s = Str.replace_first (Str.regexp "^ *") "" s in
  let s = Str.replace_first (Str.regexp " *$") "" s in
  s

(*
 * Config file format:
 * # Some comment. Indicate by a pound sign at the start of a new line
 * key = a possibly space-separated value
 *)
let parse fn =
  let contents = cat fn in
  let lines = Str.split (Str.regexp "\n") contents in
  List.fold_left (fun acc line ->
    if trim line = "" || (String.length line > 0 && line.[0] = '#') then acc
    else
      let parts = Str.bounded_split (Str.regexp "=") line 2 in
      match parts with
      | [k; v] -> SMap.add (trim k) (trim v) acc
      | _ -> raise (Failure ("failed to parse config file "^fn));
  ) SMap.empty lines
