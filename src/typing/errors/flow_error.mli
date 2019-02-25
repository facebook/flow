(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t

val loc_of_error : 'loc t -> 'loc option
val msg_of_error : 'loc t -> Error_message.t
val source_file : 'loc t -> File_key.t
val trace_reasons : 'loc t -> Reason.t list

val error_of_msg :
   trace_reasons:Reason.t list ->
   source_file:File_key.t ->
   Error_message.t -> ALoc.t t

val make_error_printable : ALoc.t t -> ALoc.t Errors.printable_error

val ordered_reasons : Reason.t * Reason.t -> Reason.t * Reason.t
