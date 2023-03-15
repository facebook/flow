(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val make :
  n:int ->
  channel_mode:[ `pipe | `socket ] ->
  gc_control:Gc.control ->
  init_id:string ->
  SharedMem.handle ->
  MultiWorkerLwt.worker list
