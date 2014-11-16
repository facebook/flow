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
(* Very simple module used to make sure we don't mix keys of different
 * type in the heap (cf shared.ml).
 * Because we have to "create" a new module every time, we have to make a new
 * prefix (cf prefix.ml). Since the prefixes are always different (for each
 * call to make), we are sure that they are not colliding.
*)
(*****************************************************************************)

module type Type = sig
  type t
  val prefix: Prefix.t
end

