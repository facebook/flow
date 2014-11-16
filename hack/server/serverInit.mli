(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* will parse, name, typecheck, the next set of files
 * and refresh the environment and update the many shared heaps
 *)
val init:
  ServerEnv.genv -> ServerEnv.env -> Relative_path.t MultiWorker.nextlist ->
  ServerEnv.env
