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

type ide_to_typechecker_message =
  (* Let typechecker process know that we are done accessing shared heap until
   * the next RunIdeCommands is received *)
  | IdeCommandsDone
