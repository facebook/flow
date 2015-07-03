(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let log_raw s =
  let open Unix in
  let tm = localtime (time ()) in
  let year = tm.tm_year + 1900 in
  let time_str = spf "[%d-%02d-%02d %02d:%02d:%02d]"
    year (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec in
  Printf.eprintf "%s %s%!" time_str s

(* wraps log_raw in order to take a format string & add a newline *)
let log fmt = Printf.ksprintf log_raw (fmt^^"\n")

let measure name f =
  log_raw (name ^ " ... ");
  let t = Unix.gettimeofday() in
  let result = f () in
  let t2 = Unix.gettimeofday() in
  Printf.eprintf "%f\n%!" (t2 -. t);
  result
