(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t

val loc_of_error : 'loc t -> 'loc option

val msg_of_error : 'loc t -> 'loc Error_message.t'

val code_of_error : 'loc t -> Error_codes.error_code option

val source_file : 'loc t -> File_key.t

val trace_reasons : 'loc t -> 'loc Reason.virtual_reason list

val kind_of_error : 'loc t -> Flow_errors_utils.error_kind

val error_of_msg :
  trace_reasons:'loc Reason.virtual_reason list ->
  source_file:File_key.t ->
  'loc Error_message.t' ->
  'loc t

val ordered_reasons : Reason.t * Reason.t -> Reason.t * Reason.t

module ErrorSet : Flow_set.S with type elt = ALoc.t t

module ConcreteErrorSet : Flow_set.S with type elt = Loc.t t

val map_loc_of_error : ('a -> 'b) -> 'a t -> 'b t

val post_process_errors : ErrorSet.t -> ErrorSet.t

val make_error_printable :
  ('loc -> Loc.t) ->
  strip_root:File_path.t option ->
  ?speculation:bool ->
  'loc t ->
  Loc.t Flow_errors_utils.printable_error

val make_errors_printable :
  (ALoc.t -> Loc.t) ->
  strip_root:File_path.t option ->
  ErrorSet.t ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
