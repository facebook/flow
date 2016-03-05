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
open Coverage_level

module C = Tty

(*****************************************************************************)
(* Section defining the colors we are going to use *)
(*****************************************************************************)

let err_clr       = C.Bold C.Red       (* Unchecked code *)
let checked_clr   = C.Normal C.Green   (* Checked code *)
let partial_clr   = C.Normal C.Yellow  (* Partially checked code *)
let default_color = C.Normal C.Default (* All the rest *)

let replace_color input =
  match input with
  | (Some Unchecked, str) -> (err_clr, str)
  | (Some Checked, str) -> (checked_clr, str)
  | (Some Partial, str) -> (partial_clr, str)
  | (None, str) -> (default_color, str)

let replace_colors input =
  List.map input replace_color

(*****************************************************************************)
(* The entry point. *)
(*****************************************************************************)

let go file_input output_json pos_level_l =
  let str = match file_input with
    | ServerUtils.FileName filename -> Sys_utils.cat filename
    | ServerUtils.FileContent content -> content
  in
  let results = ColorFile.go str pos_level_l in
  if output_json then
    print_endline (Hh_json.json_to_string (ServerColorFile.to_json results))
  else if Unix.isatty Unix.stdout
  then C.cprint (replace_colors results)
  else print_endline str
