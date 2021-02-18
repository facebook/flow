(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val compare : Type.trace -> Type.trace -> int

val trace_depth : Type.trace -> int

val unit_trace : Type.t -> Type.use_t -> Type.trace

val rec_trace : max:int -> Type.t -> Type.use_t -> Type.trace -> Type.trace

val concat_trace : max:int -> Type.trace list -> Type.trace

val dummy_trace : Type.trace

val reasons_of_trace : ?level:int -> Type.trace -> Reason.reason list
