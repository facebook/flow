(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Saves the state that load_mini_script below reads in *)
val save_state: ServerEnv.env -> string -> unit

(* will parse, name, typecheck, the next set of files
 * and refresh the environment and update the many shared heaps
 *)
val init: ?load_mini_script:Path.t -> ServerEnv.genv
  -> ServerEnv.env * bool (* whether the script succeeded *)
