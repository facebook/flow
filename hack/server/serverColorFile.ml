(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = ((int * int) * Coverage_level.level) list

let get_level_list check =
  let type_acc = Hashtbl.create 0 in
  let fn = Typing.with_expr_hook
    (fun (p, _) ty -> Hashtbl.replace type_acc p ty) check in
  let level_of_type = Coverage_level.level_of_type_mapper fn in
  let result = Hashtbl.fold (fun p ty xs ->
    (Pos.info_raw p, level_of_type (p, ty)) :: xs) type_acc [] in
  result

let go env f_in =
  get_level_list begin fun () ->
    ServerIdeUtils.check_file_input env.ServerEnv.nenv env.ServerEnv.files_info f_in
  end
