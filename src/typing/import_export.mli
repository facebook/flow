(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_module_t : Context.t -> ?perform_platform_validation:bool -> ALoc.t * string -> Type.t
