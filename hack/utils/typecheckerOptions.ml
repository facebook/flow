(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  assume_php : bool;
  (* For somewhat silly historical reasons having to do with the lack of
   * .hhi's for fairly core XHP classes, we unfortunately mark all XHP
   * classes as not having their members fully known *)
  unsafe_xhp : bool;
}

let empty = {
  assume_php = true;
  unsafe_xhp = false;
}

let assume_php t = t.assume_php
let unsafe_xhp t = t.unsafe_xhp
