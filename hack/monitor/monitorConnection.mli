(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val server_exists : string -> bool

val connect_once: ServerMonitorUtils.monitor_config -> string ->
  (Timeout.in_channel * out_channel, ServerMonitorUtils.connection_error) Result.t

val connect_and_shut_down: ServerMonitorUtils.monitor_config ->
  (ServerMonitorUtils.shutdown_result, ServerMonitorUtils.connection_error)
  Result.t
