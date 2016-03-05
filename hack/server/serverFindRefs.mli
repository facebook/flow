(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open FindRefsService

val to_json: result -> Hh_json.json

val get_refs_with_defs : action -> ServerEnv.genv ->
  ServerEnv.env -> (Naming_heap.FunHeap.key * Pos.t) list

val go : action -> ServerEnv.genv -> ServerEnv.env -> result
