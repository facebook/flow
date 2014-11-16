(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = ServerMsg.patch list

let go action genv env oc =
  let find_refs_action, new_name = match action with
    | ServerMsg.ClassRename (old_name, new_name) ->
        ServerMsg.Class old_name, new_name
    | ServerMsg.MethodRename (class_name, old_name, new_name) ->
        ServerMsg.Method (class_name, old_name), new_name
    | ServerMsg.FunctionRename (old_name, new_name) ->
        ServerMsg.Function old_name, new_name in
  
  let refs = ServerFindRefs.get_refs_with_defs find_refs_action genv env in
  let changes = List.fold_left begin fun acc x ->
    let replacement = {
                        ServerMsg.pos  = Pos.to_absolute (snd x);
                        ServerMsg.text = new_name;
                      } in
    let patch = ServerMsg.Replace replacement in            
    patch :: acc
  end [] refs in
  Marshal.to_channel oc (changes : result) [];
  flush oc
