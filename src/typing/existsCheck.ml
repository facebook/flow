(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  null_loc: Loc.t option;
  bool_loc: Loc.t option;
  string_loc: Loc.t option;
  number_loc: Loc.t option;
  mixed_loc: Loc.t option;
}

let empty = {
  null_loc = None;
  bool_loc = None;
  string_loc = None;
  number_loc = None;
  mixed_loc = None;
}
