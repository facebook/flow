(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

type result = Pos.absolute list

let go content line char =
  let get_result = FindLocalsService.attach_hooks line char in
  let funs, classes =
    ServerIdeUtils.declare_and_check Relative_path.default content in
  ServerIdeUtils.revive funs classes;
  FindLocalsService.detach_hooks ();
  List.map (get_result ()) Pos.to_absolute
