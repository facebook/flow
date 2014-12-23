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

let go env (fn, line, char) oc =
  let clean () =
    Typing_defs.infer_type := None;
    Typing_defs.infer_target := None;
    Typing_defs.infer_pos := None;
  in
  clean ();
  Typing_defs.infer_target := Some (line, char);
  ServerIdeUtils.check_file_input env.ServerEnv.files_info fn;
  let pos = opt_map Pos.to_absolute !Typing_defs.infer_pos in
  let ty = !Typing_defs.infer_type in
  clean ();
  Marshal.to_channel oc ((pos, ty) : result) [];
  flush oc
