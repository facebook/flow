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

type result = Pos.absolute option * string option

let go (env:ServerEnv.env) (fn, line, char) =
  let get_result = InferAtPosService.attach_hooks line char in
  ignore (ServerIdeUtils.check_file_input env.ServerEnv.nenv env.ServerEnv.files_info fn);
  let pos, ty = get_result () in
  let pos = opt_map Pos.to_absolute pos in
  InferAtPosService.detach_hooks ();
  pos, ty
