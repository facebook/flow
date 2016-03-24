(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(* This module does the same thing as
 * HackSearchService.MasterApi.update_search_index, but allows to process
 * updates in small chunks instead of blocking the process for the entire
 * duration of index construction *)

let batch_size = 1000
let pending_updates = ref 0

let updates_pending () = !pending_updates > 0

let enqueue_updates files =
  let files = ref files in
  while List.length !files <> 0 do
    let files_to_process, files_remaining = List.split_n !files batch_size in
    files := files_remaining;
    incr pending_updates;
    (* TODO: abstract over the locking *)
    IdeScheduler.wait_for_fun
      (fun _ -> SharedMem.hashtable_mutex_trylock ())
      begin fun env ->
        Utils.with_context
          ~enter:(fun () -> ())
          ~do_:(fun () ->
            HackSearchService.SS.MasterApi.update_search_index
              files_to_process;
            decr pending_updates;
          )
          ~exit:(fun () -> SharedMem.hashtable_mutex_unlock ());
        env
      end
      ~once:true
      ~priority:IdePriorities.idle
  done
