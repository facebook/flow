(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error_kind =
  | ParseError
  | InferError
  | InferWarning
  | InternalError
  | DuplicateProviderError
  | RecursionLimitError
  | LintError of Lints.lint_kind

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

module Friendly : sig
  type message = message_feature list

  and message_feature =
    | Inline of message_inline list
    | Reference of message_inline list * Loc.t

  and message_inline =
    | Text of string
    | Code of string

  val message_of_string: string -> message
  val text: string -> message_feature
  val code: string -> message_feature
  val ref: ?loc:bool -> Reason.reason -> message_feature
  val conjunction_concat: ?conjunction:string -> message list -> message
  val capitalize: message -> message
end

(* error structure *)

type error

val mk_error:
  ?kind:error_kind ->
  ?trace_infos:info list ->
  ?extra:info_tree list ->
  info list ->
  error

val mk_friendly_error:
  ?kind:error_kind ->
  ?trace_infos:info list ->
  Loc.t ->
  Friendly.message ->
  error

val mk_friendly_error_with_root:
  ?kind:error_kind ->
  ?trace_infos:info list ->
  (Loc.t * Friendly.message) ->
  (Loc.t * Friendly.message) ->
  error

val is_duplicate_provider_error: error -> bool

val loc_of_error: error -> Loc.t
val locs_of_error: error -> Loc.t list
val infos_of_error: error -> info list
val extra_of_error: error -> info_tree list
val kind_of_error: error -> error_kind

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet : Set.S with type elt = error

(* formatters/printers *)

type stdin_file = (Path.t * string) option

val deprecated_json_props_of_loc :
  strip_root: Path.t option ->
  Loc.t ->
  (string * Hh_json.json) list

(* Some of the error printing functions consist only of named and optional arguments,
 * requiring an extra unit argument for disambiguation on partial application. For
 * consistency, the extra unit has been adopted on all error printing functions. *)

(* Human readable output *)
module Cli_output : sig
  type error_flags = {
    color: Tty.color_mode;
    include_warnings: bool;
    max_warnings: int option;
    one_line: bool;
    show_all_errors: bool;
  }

  val default_error_flags: error_flags

  val print_errors:
    out_channel:out_channel ->
    flags:error_flags ->
    ?stdin_file:stdin_file ->
    strip_root: Path.t option ->
    errors: ErrorSet.t ->
    warnings: ErrorSet.t ->
    unit ->
    unit

  (* Simplified interface for ease of debugging; not suitable for production use *)
  val string_of_error: error -> string
end

module Json_output : sig
  val json_of_errors_with_context :
    strip_root: Path.t option ->
    stdin_file: stdin_file ->
    suppressed_errors: (error * Utils_js.LocSet.t) list ->
    errors: ErrorSet.t ->
    warnings: ErrorSet.t ->
    unit ->
    Hh_json.json

  val full_status_json_of_errors :
    strip_root: Path.t option ->
    suppressed_errors: (error * Utils_js.LocSet.t) list ->
    ?profiling:Profiling_js.finished option ->
    ?stdin_file:stdin_file ->
    errors: ErrorSet.t ->
    warnings: ErrorSet.t ->
    unit ->
    Hh_json.json

  val print_errors:
    out_channel:out_channel ->
    strip_root: Path.t option ->
    suppressed_errors: (error * Utils_js.LocSet.t) list ->
    ?pretty:bool ->
    ?profiling:Profiling_js.finished option ->
    ?stdin_file:stdin_file ->
    errors: ErrorSet.t ->
    warnings: ErrorSet.t ->
    unit ->
    unit
end

module Vim_emacs_output : sig
  val string_of_loc:
    strip_root:Path.t option ->
    Loc.t -> string
  val print_errors:
    strip_root:Path.t option ->
    out_channel ->
    errors:ErrorSet.t ->
    warnings:ErrorSet.t ->
    unit ->
    unit
end
