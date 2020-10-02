(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type reporter_options = {
  strip_root: Path.t option;
  exact_by_default: bool;
}

type 'a reporter =
  | StringReporter of (reporter_options -> 'a -> string)
  | UnitReporter of (reporter_options -> 'a -> unit)

type 'a t = {
  report: 'a reporter;
  combine: 'a -> 'a -> 'a;
  empty: 'a;
}

let unit_reporter =
  let report = StringReporter (fun _ _ -> "") in
  let combine _ _ = () in
  let empty = () in
  { report; combine; empty }
