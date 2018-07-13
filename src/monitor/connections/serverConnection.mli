(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include FlowServerMonitorConnection.CONNECTION
  with type in_message := MonitorProt.server_to_monitor_message
  and type out_message := MonitorProt.monitor_to_server_message
