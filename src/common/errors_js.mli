(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)


val pos_range : Pos.t -> int * int * int * int

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

(******* Error output functionality working on Hack's error *******)

val print_errorl : bool -> Errors.error list -> out_channel -> unit
