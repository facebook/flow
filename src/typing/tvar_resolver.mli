(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val resolve : Context.t -> Type.t -> unit

val resolved_t : Context.t -> Type.t -> Type.t
