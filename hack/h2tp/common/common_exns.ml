(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module List = List_ext
open Utils

(* Errors that caught before the main loop *)
(* Incomplete implementation *)
exception Todo of string

(* Error thrown by hack_format *)
exception FormatterError of string


(* Other Errors *)
(* Never expected to occur. This is an invariant *)
exception Impossible

(* Invariant with a Message *)
exception AssertionError of string

(* Some underlying component threw an error *)
exception InternalError of string

(* error from underlying os *)
exception CmdError of Unix.process_status * string

(* error for file operation *)
exception FileExists of string

(* Error during conversion of process *)
exception ConversionError of Pos.t * string

(* Errors while parsing *)
exception ParseErrors of Errors.error list

(* Error with provided input *)
exception InputError of string

(* Container for many errors to bundle them *)
exception CompoundError of exn list

let invariant cond error_part =
  if not cond then
    Errors.make_error [error_part] |>
    Errors.to_absolute |>
    Errors.to_string |>
    fun s -> raise (AssertionError s)

let rec flatten_error = function
  | InputError m -> [("Input Error", m)]
  | InternalError m | CmdError (_, m) -> [("Internal Error", m)]
  | ConversionError (p, m) ->
      let e = Errors.to_absolute (Errors.make_error [(p, m)]) in
      [("Conversion Error", Errors.to_string e)]
  | CompoundError exns ->
      compose (List.concatMap flatten_error) List.rev exns
  | ParseErrors errors ->
      List.map (compose Errors.to_string Errors.to_absolute) errors |>
      String.concat "\n" |>
      fun m -> [("Parse Error", m)]
  | _ -> [("Unknown Error", "")]
