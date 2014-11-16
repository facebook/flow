(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let fork ?reason:(reason=None) () =
  let result = Unix.fork() in
  (match result with
  | -1 | 0 -> ()
  | pid -> PidLog.log ~reason pid);
  result
