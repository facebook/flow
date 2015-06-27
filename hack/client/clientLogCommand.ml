(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 (* The full type ClientCommand.command refers to environment types in
  * other client modules like ClientStart.env, ClientBuild.env, etc. If
  * we want to do logging from, e.g. inside ClientBuild, then the fact
  * that EventLogger's logging functions take the current client
  * command as an argument, this creates a circular dependency
  *
  * ClientBuild -> EventLogger -> ClientCommand
  *      ^-------------------------------v
  *
  * To avoid this, we have here a stripped-down version of
  * ClientCommand.command where the data carried by each branch is only
  * the data required for logging. *)

type build_kind =
  | Push
  | Full
  | Incremental
  | Steps

type log_command =
  | LCCheck of Path.t * (* from *) string * (* mode *) string
  | LCStart of Path.t
  | LCStop of Path.t
  | LCRestart of Path.t
  | LCBuild of Path.t * build_kind
  | LCProlog of Path.t
