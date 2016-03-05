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
  | TypecheckerInitDone
  (* New FileInfo for a subset of paths. Typechecker never removes paths from
   * the map (deleted files are just updated with empty info), so all
   * updates are of the from "overwrite previous values for those paths with
   * new ones" *)
  | SyncFileInfo of FileInfo.t Relative_path.Map.t
  | SyncErrorList of Errors.t
  (* See comment on FindRefsCall *)
  | FindRefsResponse of (IdeJson.call_id * FindRefsService.result)

type ide_to_typechecker_message =
  (* Finding all references is a heavyweight action that (in some cases)
   * must be parallelized and will take multiple seconds anyway, so we don't
   * want to do it in IDE process. Sending this message enqueues the request
   * to be done by the typechecker process. *)
  | FindRefsCall of (IdeJson.call_id * FindRefsService.action)
