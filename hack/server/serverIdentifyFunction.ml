(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let go content line char tcopt =
  let result = ref None in
  IdentifySymbolService.attach_hooks result line char;
  let funs, classes =
    ServerIdeUtils.declare_and_check Relative_path.default content in
  let result =
    Option.map !result (IdentifySymbolService.infer_method_position tcopt) in
  ServerIdeUtils.revive funs classes;
  IdentifySymbolService.detach_hooks ();
  result

let go_absolute content line char tcopt =
  Option.map (go content line char tcopt) IdentifySymbolService.to_absolute
