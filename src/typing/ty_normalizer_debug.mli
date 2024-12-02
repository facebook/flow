(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * A debugging facility for getting quick string representations of Type.t.
 * Should not be used in any user visible code.
 *)
val debug_string_of_t : Context.t -> Type.t -> string
