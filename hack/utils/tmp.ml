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

let temp_dir_name =
  let dir = Filename.temp_dir_name in
  if dir.[String.length dir - 1] <> '/' then dir ^ "/" else dir

let get_dir ?user:(user=None) () =
  let user = match user with
    | None -> Sys.getenv "USER"
    | Some user -> user in
  let tmp_dir = temp_dir_name ^ SysConfig.temp_base ^ "_" ^ user in
  if not (Sys.file_exists tmp_dir)
  then Unix.mkdir tmp_dir 0o755;
  tmp_dir

(* The missing counterpart to Filename.temp_file. Put in a random location
 * under get_dir() above. *)
let temp_dir prefix  =
  let tmpdir =
    Printf.sprintf "%s/%s_%06x" (get_dir ()) prefix (Random.bits ()) in
  Unix.mkdir tmpdir 0o755;
  tmpdir
