(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(** This is a simple module that just prints messages to stderr. The nice thing
 * about it is that it can be disabled using some global state. This way the
 * server can log messages to stderr when running in detached or server mode,
 * but not when its running in check mode *)

let enabled = ref true

let disable () = enabled := false

let oc_list = ref [ stderr ]

let also_log_to_fd fd =
  let oc = Unix.out_channel_of_descr fd in
  oc_list := oc::!oc_list

let log_raw s =
  if !enabled
  then
    let open Unix in
    let tm = localtime (time ()) in
    let year = tm.tm_year + 1900 in
    let time_str = spf "[%d-%02d-%02d %02d:%02d:%02d]"
      year (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec in
    List.iter (fun oc -> Printf.fprintf oc "%s %s%!" time_str s) !oc_list

(* wraps log_raw in order to take a format string & add a newline *)
let log fmt = Printf.ksprintf log_raw (fmt^^"\n")
