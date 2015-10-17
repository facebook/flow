(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Used for integration with HHVM *)

let stamp_file = Filename.concat GlobalConfig.tmp_dir "stamp"

let touch_stamp () =
  Sys_utils.mkdir_no_fail (Filename.dirname stamp_file);
  Sys_utils.with_umask
    0o111
    (fun () ->
     (* Open and close the file to set its mtime. Don't use the Unix.utimes
      * function since that will fail if the stamp file doesn't exist. *)
     close_out (open_out stamp_file)
    )

let touch_stamp_errors l1 l2 =
  (* We don't want to needlessly touch the stamp file if the error list is
   * the same and nothing has changed, but we also don't want to spend a ton
   * of time comparing huge lists of errors over and over (i.e., grind to a
   * halt in the cases when there are thousands of errors). So we cut off
   * the comparison at an arbitrary point. *)
  let rec length_greater_than n = function
    | [] -> false
    | _ when n = 0 -> true
    | _::l -> length_greater_than (n-1) l in
  if length_greater_than 5 l1 || length_greater_than 5 l2 || l1 <> l2
  then touch_stamp ()
