(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
