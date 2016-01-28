(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type command =
  | CCheck of ClientEnv.client_check_env
  | CStart of ClientStart.env
  | CStop of ClientStop.env
  | CRestart of ClientStart.env
  | CBuild of ClientBuild.env
  | CIde of ClientIde.env

type command_keyword =
  | CKCheck
  | CKStart
  | CKStop
  | CKRestart
  | CKBuild
  | CKNone
  | CKIde
