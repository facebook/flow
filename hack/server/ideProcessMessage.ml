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
  (* Let IDE process know that it's safe to access shared heap now *)
  | RunIdeCommands
  (* Let IDE process know that it should pause accessing shared heap. After
   * sending it, you need to wait for IDE process to finish and confirm it by
   * sending IdeCommandsDone before proceeding *)
  | StopIdeCommands
  (* New FileInfo for a subset of paths. Typechecker never removes paths from
   * the map (deleted files are just updated with empty info), so all
   * updates are of the from "overwrite previous values for those paths with
   * new ones" *)
  | SyncFileInfo of FileInfo.t Relative_path.Map.t
  | SyncErrorList of Errors.t

type ide_to_typechecker_message =
  (* Let typechecker process know that we are done accessing shared heap until
   * the next RunIdeCommands is received *)
  | IdeCommandsDone
