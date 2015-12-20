(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go content line char =
  let result = ref None in
  IdentifySymbolService.attach_hooks result line char;
  let funs, classes =
    ServerIdeUtils.declare Relative_path.default content in
  ServerIdeUtils.fix_file_and_def funs classes;
  ServerIdeUtils.revive funs classes;
  IdentifySymbolService.detach_hooks ();
  match !result with
  | Some result -> Utils.strip_ns result.IdentifySymbolService.name
  | _ -> ""
