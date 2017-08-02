(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let timestamp_string () =
  let open Unix in
  let tm = localtime (time ()) in
  let year = tm.tm_year + 1900 in
  Printf.sprintf "[%d-%02d-%02d %02d:%02d:%02d]"
    year (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let log_raw s =
  Printf.eprintf "%s %s%!" (timestamp_string ()) s

(* wraps log_raw in order to take a format string & add a newline *)
let log fmt = Printf.ksprintf log_raw (fmt^^"\n")

let log_duration name t =
  log_raw (name ^ ": ");
  let t2 = Unix.gettimeofday() in
  Printf.eprintf "%f\n%!" (t2 -. t);
  t2

let exc ?(prefix="") e =
  log_raw (prefix ^ Printexc.to_string e ^ "\n");
  Printexc.print_backtrace stderr;
  ()

module Level : sig
  type t =
    | Off
    | Fatal
    | Error
    | Warn
    | Info
    | Debug
  val min_level : unit -> t
  val set_min_level : t -> unit
  val log : t -> ('a, unit, string, string, string, unit) format6 -> 'a
end = struct
  type t =
    | Off
    | Fatal
    | Error
    | Warn
    | Info
    | Debug

  let int_of_level = function
    | Off -> 6
    | Fatal -> 5
    | Error -> 4
    | Warn -> 3
    | Info -> 2
    | Debug -> 1

  let min_level_ref = ref Info
  let min_level () = !min_level_ref
  let set_min_level level = min_level_ref := level

  let log level fmt =
    if int_of_level level >= int_of_level !min_level_ref
    then log fmt
    else Printf.ifprintf () fmt
end

let fatal fmt = Level.log Level.Fatal fmt
let error fmt = Level.log Level.Error fmt
let warn fmt = Level.log Level.Warn fmt
let info fmt = Level.log Level.Info fmt
let debug fmt = Level.log Level.Debug fmt
