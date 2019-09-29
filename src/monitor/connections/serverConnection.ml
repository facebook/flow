(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module wraps the pipe connection between the Flow server monitor and the server *)

include FlowServerMonitorConnection.Make (struct
  type in_message = MonitorProt.server_to_monitor_message

  type out_message = MonitorProt.monitor_to_server_message
end)
