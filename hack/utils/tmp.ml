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

(* Emulate "mkdir -p", i.e., no error if already exists. *)
let mkdir tmp_dir =
  Sys_utils.with_umask 0 begin fun () ->
    (* Don't set sticky bit since the socket opening code wants to remove any
     * old sockets it finds, which may be owned by a different user. *)
    try Unix.mkdir tmp_dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* The missing counterpart to Filename.temp_file. Put in a random location
 * under get_dir() above. *)
let temp_dir parent_dir prefix  =
  mkdir parent_dir;
  let tmpdir = Printf.sprintf "%s/%s_%06x" parent_dir prefix (Random.bits ()) in
  Unix.mkdir tmpdir 0o755;
  tmpdir
