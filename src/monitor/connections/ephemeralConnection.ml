(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module wraps the socket connection between the Flow server monitor and one of the short-
 * lived clients. *)

include FlowServerMonitorConnection.Make (struct
  type in_message = ServerProt.Request.command_with_context

  type out_message = MonitorProt.monitor_to_client_message
end)
