(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let hash no_flowlib : string =
  if no_flowlib then
    [%prelude_hash]
  else
    [%flowlib_hash]

let contents no_flowlib : (string * string) array =
  if no_flowlib then
    [%prelude_contents]
  else
    [%flowlib_contents]

let mkdir ~no_flowlib parent_dir =
  let libdir = Path.concat parent_dir (Printf.sprintf "flowlib_%s" (hash no_flowlib)) in
  Sys_utils.mkdir_no_fail (Path.to_string parent_dir);
  Sys_utils.mkdir_no_fail (Path.to_string libdir);
  libdir

let write_flowlib dir (filename, contents) =
  let file = Path.(concat dir filename |> to_string) in
  Sys_utils.write_file ~file contents

let extract ~no_flowlib dir = Array.iter (write_flowlib dir) (contents no_flowlib)
