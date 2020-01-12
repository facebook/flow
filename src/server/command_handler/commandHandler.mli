(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val enqueue_or_handle_ephemeral :
  ServerEnv.genv -> MonitorProt.request_id * ServerProt.Request.command_with_context -> unit Lwt.t

val enqueue_persistent :
  ServerEnv.genv -> LspProt.client_id -> LspProt.request_with_metadata -> unit Lwt.t
