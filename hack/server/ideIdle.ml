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

let task_list = ref []

let is_ready (has_work, _) = has_work ()

let go () =
  match List.find !task_list is_ready with
  | Some (_, task) -> task ()
  | None -> ()

let add_task has_work do_work =
  task_list := (has_work, do_work) :: !task_list

let has_tasks () =
  List.exists !task_list is_ready

let init () =
  add_task
    HackSearchService.IdeProcessApi.updates_pending
    HackSearchService.IdeProcessApi.process_updates
