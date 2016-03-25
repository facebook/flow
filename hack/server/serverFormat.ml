(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go content from to_ =
  let modes = [Some FileInfo.Mstrict; Some FileInfo.Mpartial] in
  Format_hack.region modes Path.dummy_path from to_ content
