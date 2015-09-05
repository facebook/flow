(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Code checking the health of the server *)
(*****************************************************************************)
open ServerEnv

(*****************************************************************************)
(* The list of pipes to select on, if any of them dies, we need to die.
 * If the user sends a "kill -9" to any subprocess (datanode or worker),
 * all the processes should die.
 * This function gives the list of pipes to watch. If anything happened
 * to a subprocess, one of these pipes will be broken.
*)
(*****************************************************************************)

let get_subproc_fdl genv =
  match genv.workers with
  | None -> []
  | Some workers ->
      let workers_pipes = List.map Worker.get_file_descr workers in
      workers_pipes

let check genv =
  (* TODO *)
  ()
