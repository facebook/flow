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
  let (funs, classes, typedefs), ast =
    ServerIdeUtils.declare_and_check_get_ast Relative_path.default content in
  let result = Option.map !result begin fun x ->
    let name_pos = ServerSymbolDefinition.go tcopt ast x in
    { x with IdentifySymbolService.name_pos = name_pos }
  end in
  ServerIdeUtils.revive funs classes typedefs;
  IdentifySymbolService.detach_hooks ();
  result

let go_absolute content line char tcopt =
  Option.map (go content line char tcopt) IdentifySymbolService.to_absolute
