(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val trace_depth : Type.trace -> int

val unit_trace : Type.trace

val rec_trace : Type.trace -> Type.trace

val concat_trace : Type.trace list -> Type.trace

val dummy_trace : Type.trace
