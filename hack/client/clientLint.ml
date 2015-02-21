(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go (results : ServerLint.result) output_json =
  if output_json then
    ServerLint.output_json stdout results
  else
    ServerLint.output_text stdout results
