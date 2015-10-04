(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = (string * Pos.absolute) list

val get_refs_with_defs : ServerMsg.find_refs_action -> ServerEnv.genv ->
  ServerEnv.env -> (Naming_heap.FunHeap.key * Pos.t) list

val go : ServerMsg.find_refs_action -> ServerEnv.genv -> ServerEnv.env ->
  out_channel -> unit
