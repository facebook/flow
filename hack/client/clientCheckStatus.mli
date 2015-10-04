(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val check_status :
  (* function to try to connect to server; we take this as a function
   * so we can, e.g., autostart a server before trying to connect *)
  (ClientEnv.client_check_env -> in_channel * out_channel)
  (* environment *)
  -> ClientEnv.client_check_env
  -> unit
