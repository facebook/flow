(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env = {
  root: Path.t;
  wait: bool;
  no_load: bool;
}

val main : env -> Exit_status.t
val start_server : env -> unit
