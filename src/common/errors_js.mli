(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type level = ERROR | WARNING
type message = (Reason_js.reason * string)
type error = level * message list * message list

type flags = {
  color: Tty.color_mode;
  one_line: bool;
  show_all_errors: bool;
}

val default_flags : flags

val format_reason_color: ?first:bool -> ?one_line:bool -> Loc.t * string ->
  (Tty.style * string) list

val print_reason_color:
  first:bool ->
  one_line:bool ->
  color:Tty.color_mode ->
  message ->
  unit

val print_error_color:
  one_line:bool -> color:Tty.color_mode -> error -> unit

val file_of_error : error -> string

val json_of_loc : Loc.t -> (string * Hh_json.json) list

module Error :
  sig
    type t = error
    val compare : error -> error -> int
  end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet : Set.S with type elt = error

module ErrorSuppressions : sig
  type t

  val empty : t
  val add : Loc.t -> t -> t
  val union : t -> t -> t
  val check : error -> t -> (bool * t)
  val unused : t -> Loc.t list
end

val parse_error_to_flow_error : (Loc.t * Parse_error.t) -> error

val to_list : ErrorSet.t -> error list

val json_of_errors : Error.t list -> Hh_json.json

(******* Error output functionality working on Hack's error *******)

val print_errorl : bool -> error list -> out_channel -> unit

(* Human readable output *)
val print_error_summary: flags:flags -> error list -> unit
