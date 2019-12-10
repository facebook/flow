(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let contents no_flowlib =
  if no_flowlib then
    [%prelude_contents]
  else
    [%flowlib_contents]

let write_flowlib dir (filename, contents) =
  let file = Path.(concat dir filename |> to_string) in
  Sys_utils.write_file ~file contents

let extract_flowlib ~no_flowlib dir = Array.iter (write_flowlib dir) (contents no_flowlib)
