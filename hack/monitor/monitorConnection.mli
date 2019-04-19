(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

val server_exists : string -> bool

val connect_once:
  timeout:int ->
  ServerMonitorUtils.monitor_config ->
  MonitorRpc.handoff_options ->
  (Timeout.in_channel * out_channel * string, ServerMonitorUtils.connection_error) result

val connect_and_shut_down: ServerMonitorUtils.monitor_config ->
  (ServerMonitorUtils.shutdown_result, ServerMonitorUtils.connection_error)
  result

val connect_to_monitor_and_get_server_progress:
  timeout:int ->
  ServerMonitorUtils.monitor_config ->
  (string option * string option, ServerMonitorUtils.connection_error) result
