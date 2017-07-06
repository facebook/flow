(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(** Performs a map, but returns the original optional if there is no change **)
let ident_map f x =
  match x with
  | None -> x
  | Some x' ->
      let x'' = f x' in
      if x'' == x' then x
      else Some x''
