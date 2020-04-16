(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a reporter =
  | StringReporter of (strip_root:Path.t option -> 'a -> string)
  | UnitReporter of (strip_root:Path.t option -> 'a -> unit)

type 'a t = {
  report: 'a reporter;
  combine: 'a -> 'a -> 'a;
  empty: 'a;
}

let unit_reporter =
  let report = StringReporter (fun ~strip_root:_ _ -> "") in
  let combine _ _ = () in
  let empty = () in
  { report; combine; empty }
