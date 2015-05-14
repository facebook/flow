(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type result = int * (string option * string) list

let go genv env content line char =
  ArgumentInfoService.attach_hooks (line, char);
  let funs, classes = ServerIdeUtils.declare Relative_path.default content in
  ServerIdeUtils.fix_file_and_def Relative_path.default content;
  let pos, expected =
    match ArgumentInfoService.get_result() with
    | Some (pos, expected) -> pos, expected
    | _ ->(-1), []
  in
  ArgumentInfoService.detach_hooks();
  ServerIdeUtils.revive funs classes;
  pos, expected
