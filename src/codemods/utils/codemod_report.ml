(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t = {
  report: strip_root:Path.t option -> 'a -> unit;
  combine: 'a -> 'a -> 'a;
  empty: 'a;
}

let unit_reporter =
  let report ~strip_root:_ _ = () in
  let combine _ _ = () in
  let empty = () in
  { report; combine; empty }
