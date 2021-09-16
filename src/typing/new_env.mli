(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val read : Context.t -> ALoc.t -> Reason.t -> Type.t

val write : Context.t -> use_op:Type.use_op -> ALoc.t -> Type.t -> unit

val initialize_all : Context.t -> unit
