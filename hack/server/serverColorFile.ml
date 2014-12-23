(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type result = ((int * int) * Coverage_level.level) list

let go env f_in oc =
  assert (!Typing_defs.type_acc = []);
  Typing_defs.accumulate_types := true;
  ServerIdeUtils.check_file_input env.ServerEnv.files_info f_in;
  Typing_defs.accumulate_types := false;
  let fn_opt = match f_in with
    | ServerMsg.FileContent _ -> None
    | ServerMsg.FileName fn -> Some (Relative_path.create Relative_path.Root fn)
  in
  let result = Coverage_level.mk_level_list fn_opt !Typing_defs.type_acc in
  Typing_defs.type_acc := [];
  let result = rev_rev_map (fun (p, cl) -> Pos.info_raw p, cl) result in
  Marshal.to_channel oc (result : result) [];
  flush oc
