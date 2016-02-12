(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeProcessMessage

type ('a, 'b) pipe = {
  in_fd : Unix.file_descr;
  out_fd : Unix.file_descr;
}

type to_ide =
  (ide_to_typechecker_message, typechecker_to_ide_message) pipe
type to_typechecker =
  (typechecker_to_ide_message, ide_to_typechecker_message) pipe

let send pipe msg =
  Marshal_tools.to_fd_with_preamble pipe.out_fd msg

let recv pipe =
  Marshal_tools.from_fd_with_preamble pipe.in_fd
