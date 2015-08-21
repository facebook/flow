(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Handling where our temporary files go *)
(*****************************************************************************)

(* The missing counterpart to Filename.temp_file. Put in a random location
 * under get_dir() above. *)
let temp_dir parent_dir prefix  =
  Sys_utils.mkdir_no_fail parent_dir;
  let tmpdir =
    Filename.concat
      parent_dir
      (Printf.sprintf "%s_%06x" prefix (Random.bits ())) in
  Unix.mkdir tmpdir 0o755;
  tmpdir
