(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Pretty printing of types *)
(*****************************************************************************)

val error: 'a Typing_defs.ty_ -> string
val suggest: 'a Typing_defs.ty -> string
val full: Typing_env.env -> 'a Typing_defs.ty -> string
val full_strip_ns: Typing_env.env -> 'a Typing_defs.ty -> string
val debug: Typing_env.env -> 'a Typing_defs.ty -> string
val class_: Typing_heap.Class.t -> string
val gconst: Typing_heap.GConst.t -> string
val fun_: Typing_heap.Fun.t -> string
val typedef: Typing_heap.Typedef.t -> string
val strip_ns: Typing_env.env -> Typing_defs.phase_ty -> string
