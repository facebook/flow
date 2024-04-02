(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  client_logging_context: FlowEventLogger.logging_context;
  command: ServerProt.Request.command;
}
