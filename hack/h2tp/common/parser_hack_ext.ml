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
module List = List_ext
module Sys = Sys_ext
module CE = Common_exns

include Parser_hack

let parse_or_die file =
  let content = cat (Relative_path.to_absolute file) in
  let errors, res = Errors.do_
    (fun () -> Parser_hack.program file ~elaborate_namespaces:false content) in
  if (List.not_empty errors) then raise (CE.ParseErrors errors);
  res
