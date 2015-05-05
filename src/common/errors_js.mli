(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val pos_range : Pos.t -> int * int * int * int

val format_reason_color: ?first:bool -> Pos.t * string ->
  (Tty.style * string) list

val print_reason_color: first:bool -> Pos.t * string -> unit

val print_error_color: Errors.error -> unit

type level = ERROR | WARNING

type error = level * (Reason_js.reason * string) list

val file_of_error : error -> string

val pos_to_json : Pos.t -> (string * Hh_json.json) list

module Error :
  sig
    type t = error
    val compare : error -> error -> int
  end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet : Set.S with type elt = error

(******* TODO move to hack structure throughout ********)

val flow_error_to_hack_error : error -> Errors.error

val parse_error_to_flow_error :
  (Spider_monkey_ast.Loc.t * Parse_error.t) -> error

val parse_error_to_hack_error :
  (Spider_monkey_ast.Loc.t * Parse_error.t) -> Errors.error

val to_list : ErrorSet.t -> Errors.error list

(******* Error output functionality working on Hack's error *******)

val print_errorl : bool -> Errors.error list -> out_channel -> unit

(* Human readable output *)
val print_error_summary : bool -> Errors.error list -> unit
