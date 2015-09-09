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

type patch =
  | Insert of insert_patch
  | Remove of Pos.absolute
  | Replace of insert_patch

and insert_patch = {
  pos: Pos.absolute;
  text: string;
}

type action =
  | ClassRename of string * string (* old_name * new_name *)
  | MethodRename of string * string * string
    (* class_name * old_name * new_name*)
  | FunctionRename of string * string (* old_name * new_name *)

let go action genv env =
  let find_refs_action, new_name = match action with
    | ClassRename (old_name, new_name) ->
        ServerFindRefs.Class old_name, new_name
    | MethodRename (class_name, old_name, new_name) ->
        ServerFindRefs.Method (class_name, old_name), new_name
    | FunctionRename (old_name, new_name) ->
        ServerFindRefs.Function old_name, new_name in
  
  let refs = ServerFindRefs.get_refs_with_defs find_refs_action genv env in
  let changes = List.fold_left refs ~f:begin fun acc x ->
    let replacement = {
      pos  = Pos.to_absolute (snd x);
      text = new_name;
    } in
    let patch = Replace replacement in
    patch :: acc
  end ~init:[] in
  changes
