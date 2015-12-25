(**
 * Copyright (c) 2015, Facebook, Inc.
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
val rec_trace: Type.t -> Type.use_t -> t -> t
val concat_trace: t list -> t

val reasons_of_trace: ?level:int -> t -> Reason_js.reason list
