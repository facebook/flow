(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val default_resolve_touts : flow:(Type.t * Type.t -> unit) -> 'a -> ALoc.t -> Type.use_t -> unit
