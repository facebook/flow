(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* Very simple module used to make sure we don't mix keys of different
 * type in the heap (cf shared.ml).
 * Because we have to "create" a new module every time, we have to make a new
 * prefix (cf prefix.ml). Since the prefixes are always different (for each
 * call to make), we are sure that they are not colliding.
 *)
(*****************************************************************************)

module type Type = sig
  type t

  val prefix : Prefix.t

  val description : string
end
