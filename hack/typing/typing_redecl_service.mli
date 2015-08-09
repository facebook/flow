(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_deps

val redo_type_decl :
  update_pos:bool ->
  Worker.t list option ->
  Naming.env ->
  FileInfo.names Relative_path.Map.t ->
  Errors.error list * Relative_path.Set.t * DepSet.t * DepSet.t

(*
 * XXX UNUSED: Position substitution has been disabled for now, but we're
 * leaving the code in to minimize bitrot
 *)
val update_positions: Utils.SSet.t -> 'a -> unit
