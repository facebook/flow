(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

type typechecker_to_ide_message =
  | Typechecker_init_done
  (* New FileInfo for a subset of paths. Typechecker never removes paths from
   * the map (deleted files are just updated with empty info), so all
   * updates are of the from "overwrite previous values for those paths with
   * new ones" *)
  | Recheck_finished
  | Sync_file_info of FileInfo.t Relative_path.Map.t
  | Sync_error_list of Errors.t
  (* See comment on Find_refs_call *)
  | Find_refs_response of (IdeJson.call_id * FindRefsService.result)

type ide_to_typechecker_message =
  (* Finding all references is a heavyweight action that (in some cases)
   * must be parallelized and will take multiple seconds anyway, so we don't
   * want to do it in IDE process. Sending this message enqueues the request
   * to be done by the typechecker process. *)
  | Find_refs_call of (IdeJson.call_id * FindRefsService.action)
  | Start_recheck
