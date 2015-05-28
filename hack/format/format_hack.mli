(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a return =
  | Disabled_mode
  | Parsing_error of Errors.error list
  | Internal_error
  | Success of 'a

(* Absolute character position in the input file. *)
type char_pos = int

type source_tag =
  (* Line number in the input file *)
  | Line of int

  (* Beginning of an indivisible formatting block *)
  | Block

(* Meta-data to be able to reconcile the input file and the
 * formatted output (useful for Format_diff)
 *)
type source_pos = char_pos * source_tag

val region:
  FileInfo.mode option list ->
  Path.t ->
  start:int ->
  end_:int ->
  string ->
  string return

val program:
  ?no_trailing_commas:bool ->
  FileInfo.mode option list ->
  Path.t -> string ->
  string return

val program_with_source_metadata:
  FileInfo.mode option list ->
  Path.t ->
  string ->
  (string * source_pos list) return
