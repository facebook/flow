(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Performs a map, but returns the original optional if there is no change **)
let ident_map f x =
  match x with
  | None -> x
  | Some x' ->
    let x'' = f x' in
    if x'' == x' then
      x
    else
      Some x''
