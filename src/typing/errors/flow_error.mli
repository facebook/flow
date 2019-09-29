(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t

val loc_of_error : 'loc t -> 'loc option

val msg_of_error : 'loc t -> 'loc Error_message.t'

val source_file : 'loc t -> File_key.t

val trace_reasons : 'loc t -> 'loc Reason.virtual_reason list

val kind_of_error : 'loc t -> Errors.error_kind

val error_of_msg :
  trace_reasons:Reason.t list -> source_file:File_key.t -> Error_message.t -> ALoc.t t

val make_error_printable : (ALoc.t -> ALoc.table Lazy.t) -> Loc.t t -> Loc.t Errors.printable_error

val ordered_reasons : Reason.t * Reason.t -> Reason.t * Reason.t

module ErrorSet : Set.S with type elt = ALoc.t t

module ConcreteErrorSet : Set.S with type elt = Loc.t t

val make_errors_printable :
  (ALoc.t -> ALoc.table Lazy.t) -> ErrorSet.t -> Errors.ConcreteLocPrintableErrorSet.t

val map_loc_of_error : ('a -> 'b) -> 'a t -> 'b t

val concretize_error : (ALoc.t -> ALoc.table Lazy.t) -> ALoc.t t -> Loc.t t
