(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type infer_warning_kind =
  | ExportKind
  | OtherKind

type error_kind =
  | ParseError
  | PseudoParseError
  | InferError
  | InferWarning of infer_warning_kind
  | InternalError
  | DuplicateProviderError
  | RecursionLimitError
  | LintError of Lints.lint_kind

val string_of_kind : error_kind -> string

(** simple structure for callers to specify message content.
    an info list looks like e.g.:
    [ location1, ["number"; "Type is incompatible with"];
      location2, ["string"] ]
 *)
type 'a info = 'a * string list

(** for extra info, enough structure to do simple tree-shaped output *)
type 'a info_tree =
  | InfoLeaf of 'a info list
  | InfoNode of 'a info list * 'a info_tree list

module Friendly : sig
  type t

  type 'a message = 'a message_feature list

  and 'a message_feature =
    | Inline of message_inline list
    | Reference of message_inline list * 'a

  and message_inline =
    | Text of string
    | Code of string

  type docs = {
    call: string;
    tuplemap: string;
    objmap: string;
    objmapi: string;
  }

  val docs : docs

  val message_of_string : string -> 'a message

  val text : string -> 'a message_feature

  val code : string -> 'a message_feature

  val ref : ?loc:bool -> Reason.concrete_reason -> Loc.t message_feature

  val conjunction_concat : ?conjunction:string -> 'a message list -> 'a message

  val capitalize : 'a message -> 'a message
end

(* error structure *)

type 'loc printable_error

val mk_error :
  ?kind:error_kind ->
  ?trace_infos:Loc.t info list ->
  ?root:Loc.t * Loc.t Friendly.message ->
  ?frames:Loc.t Friendly.message list ->
  Loc.t ->
  Error_codes.error_code option ->
  Loc.t Friendly.message ->
  Loc.t printable_error

val mk_speculation_error :
  ?kind:error_kind ->
  ?trace_infos:Loc.t info list ->
  loc:Loc.t ->
  root:(Loc.t * Loc.t Friendly.message) option ->
  frames:Loc.t Friendly.message list ->
  error_code:Error_codes.error_code option ->
  speculation_errors:(int * Loc.t printable_error) list ->
  Loc.t printable_error

val loc_of_printable_error : 'loc printable_error -> 'loc

val locs_of_printable_error : 'loc printable_error -> 'loc list

val kind_of_printable_error : 'loc printable_error -> error_kind

val code_of_printable_error : 'loc printable_error -> Error_codes.error_code option

module ConcreteLocPrintableErrorSet : Set.S with type elt = Loc.t printable_error

(* formatters/printers *)

type stdin_file = (Path.t * string) option

val deprecated_json_props_of_loc : strip_root:Path.t option -> Loc.t -> (string * Hh_json.json) list

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
    show_all_branches: bool;
    unicode: bool;
    message_width: int;
  }

  val print_errors :
    out_channel:out_channel ->
    flags:error_flags ->
    ?stdin_file:stdin_file ->
    strip_root:Path.t option ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    lazy_msg:string option ->
    unit ->
    unit

  val format_errors :
    out_channel:out_channel ->
    flags:error_flags ->
    ?stdin_file:stdin_file ->
    strip_root:Path.t option ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    lazy_msg:string option ->
    unit ->
    unit

  (* print errors *)
end

module Json_output : sig
  type json_version =
    | JsonV1
    | JsonV2

  val json_of_errors_with_context :
    strip_root:Path.t option ->
    stdin_file:stdin_file ->
    suppressed_errors:(Loc.t printable_error * Loc_collections.LocSet.t) list ->
    ?version:json_version ->
    offset_kind:Offset_utils.offset_kind ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    unit ->
    Hh_json.json

  val full_status_json_of_errors :
    strip_root:Path.t option ->
    suppressed_errors:(Loc.t printable_error * Loc_collections.LocSet.t) list ->
    ?version:json_version ->
    ?stdin_file:stdin_file ->
    offset_kind:Offset_utils.offset_kind ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    unit ->
    profiling_props:(string * Hh_json.json) list ->
    Hh_json.json

  val print_errors :
    out_channel:out_channel ->
    strip_root:Path.t option ->
    suppressed_errors:(Loc.t printable_error * Loc_collections.LocSet.t) list ->
    pretty:bool ->
    ?version:json_version ->
    offset_kind:Offset_utils.offset_kind ->
    ?stdin_file:stdin_file ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    unit ->
    unit

  val format_errors :
    out_channel:out_channel ->
    strip_root:Path.t option ->
    suppressed_errors:(Loc.t printable_error * Loc_collections.LocSet.t) list ->
    pretty:bool ->
    ?version:json_version ->
    ?stdin_file:stdin_file ->
    offset_kind:Offset_utils.offset_kind ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    unit ->
    profiling_props:(string * Hh_json.json) list ->
    unit

  (* print errors *)
end

module Vim_emacs_output : sig
  val string_of_loc : strip_root:Path.t option -> Loc.t -> string

  val print_errors :
    strip_root:Path.t option ->
    out_channel ->
    errors:ConcreteLocPrintableErrorSet.t ->
    warnings:ConcreteLocPrintableErrorSet.t ->
    unit ->
    unit
end

module Lsp_output : sig
  type t = {
    loc: Loc.t;
    (* the file+range at which the message applies *)
    message: string;
    (* the diagnostic's message *)
    code: string;
    (* an error code *)
    relatedLocations: (Loc.t * string) list;
  }

  val lsp_of_error : Loc.t printable_error -> t
end
