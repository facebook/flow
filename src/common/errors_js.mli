(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type error_kind =
  | ParseError
  | InferError
  | InferWarning
  | InternalError

val string_of_kind: error_kind -> string

(** simple structure for callers to specify message content.
    an info list looks like e.g.:
    [ location1, ["number"; "Type is incompatible with"];
      location2, ["string"] ]
  *)
type info = Loc.t * string list

(** for extra info, enough structure to do simple tree-shaped output *)
type info_tree =
  | InfoLeaf of info list
  | InfoNode of info list * info_tree list

(* error structure *)

type error

val mk_error:
  ?kind:error_kind ->
  ?op_info:info ->
  ?trace_infos:info list ->
  ?extra:info_tree list ->
  info list ->
  error

val simple_error: ?kind: error_kind -> Loc.t -> string -> error
val internal_error: Loc.filename -> string -> error

val parse_error_to_flow_error : (Loc.t * Parse_error.t) -> error
val strip_root_from_errors: Path.t -> error list -> error list

val loc_of_error: error -> Loc.t
val infos_of_error: error -> info list
val extra_of_error: error -> info_tree list

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet : Set.S with type elt = error

val to_list : ErrorSet.t -> error list

module ErrorSuppressions : sig
  type t

  val empty : t
  val add : Loc.t -> t -> t
  val union : t -> t -> t
  val check : error -> t -> (bool * t)
  val unused : t -> Loc.t list
  val cardinal : t -> int
end

(* formatters/printers *)

type stdin_file = (Path.t * string) option

val deprecated_json_props_of_loc : Loc.t -> (string * Hh_json.json) list
val json_of_errors : error list -> Hh_json.json
val json_of_errors_with_context :
  stdin_file: stdin_file ->
  error list ->
  Hh_json.json

val print_error_color_new:
  stdin_file:stdin_file ->
  strip_root:bool ->
  one_line:bool ->
  color:Tty.color_mode ->
  root: Path.t ->
  error ->
  unit

val print_error_json :
  ?stdin_file:stdin_file ->
  out_channel ->
  error list ->
  unit

(* Human readable output *)
val print_error_summary:
  flags:Options.error_flags ->
  ?stdin_file:stdin_file ->
  strip_root: bool ->
  root: Path.t ->
  error list ->
  unit

(* used by getDef for emacs/vim output - TODO remove or undeprecate *)
val string_of_loc_deprecated: Loc.t -> string
val print_error_deprecated: out_channel -> error list -> unit

(* only used to get indentation info for trace formatting - TODO fumigate *)
val format_info: info -> string
