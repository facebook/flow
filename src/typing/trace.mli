(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val compare: t -> t -> int

val trace_depth: t -> int
val unit_trace: Type.t -> Type.use_t -> t
val rec_trace: max: int -> Type.t -> Type.use_t -> t -> t
val concat_trace: t list -> t

val reasons_of_trace:
  prep_path: (Reason.reason -> Reason.reason) ->
  ?level:int ->
  t ->
  Reason.reason list
