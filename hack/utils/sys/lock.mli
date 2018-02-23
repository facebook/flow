(**
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val grab : string -> bool
val release : string -> bool
val blocking_grab_then_release : string -> unit
val fd_of : string -> int
val check : string -> bool
