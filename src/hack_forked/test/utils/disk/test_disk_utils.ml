(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let write_file ~dir ~file ~contents =
  let file = File_path.concat dir file in
  Sys_utils.write_file ~file:(File_path.to_string file) contents

let setup_dir dir files =
  Disk.mkdir_p (File_path.to_string dir);
  List.iter (fun (file, contents) -> write_file ~dir ~file ~contents) files
