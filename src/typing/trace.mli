(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val compare : t -> t -> int

val trace_depth : t -> int

val unit_trace : Type.t -> Type.use_t -> t

val rec_trace : max:int -> Type.t -> Type.use_t -> t -> t

val concat_trace : max:int -> t list -> t

val dummy_trace : t

val reasons_of_trace : ?level:int -> t -> Reason.reason list
