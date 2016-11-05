(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t =
  | N
  | Simple
  | Argument


let to_string r =
  match r with
  | N -> "N"
  | Simple -> "Simple"
  | Argument -> "Argument"
