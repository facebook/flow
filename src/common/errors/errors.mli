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
  | DuplicateProviderError

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
val is_duplicate_provider_error: error -> bool

val loc_of_error: error -> Loc.t
val locs_of_error: error -> Loc.t list
val infos_of_error: error -> info list
val extra_of_error: error -> info_tree list

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet : Set.S with type elt = error

module ErrorSuppressions : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val add : Loc.t -> t -> t
  val union : t -> t -> t
  val check : Loc.t list -> t -> (bool * t)
  val unused : t -> Loc.t list
  val cardinal : t -> int
end

(* formatters/printers *)

type stdin_file = (Path.t * string) option

val deprecated_json_props_of_loc :
  strip_root: Path.t option ->
  Loc.t ->
  (string * Hh_json.json) list

(* Human readable output *)
module Cli_output : sig
  val print_errors:
    out_channel:out_channel ->
    flags:Options.error_flags ->
    ?stdin_file:stdin_file ->
    strip_root: Path.t option ->
    ErrorSet.t ->
    unit
end

module Json_output : sig
  val json_of_errors :
    strip_root: Path.t option ->
    ErrorSet.t ->
    Hh_json.json
  val json_of_errors_with_context :
    strip_root: Path.t option ->
    stdin_file: stdin_file ->
    ErrorSet.t ->
    Hh_json.json

  val full_status_json_of_errors :
    strip_root: Path.t option ->
    ?profiling:Profiling_js.t option ->
    ?stdin_file:stdin_file ->
    ErrorSet.t ->
    Hh_json.json

  val print_errors:
    out_channel:out_channel ->
    strip_root: Path.t option ->
    ?pretty:bool ->
    ?profiling:Profiling_js.t option ->
    ?stdin_file:stdin_file ->
    ErrorSet.t ->
    unit
end

module Vim_emacs_output : sig
  val string_of_loc:
    strip_root: Path.t option ->
    Loc.t -> string
  val print_errors:
    strip_root: Path.t option ->
    out_channel -> ErrorSet.t -> unit
end
