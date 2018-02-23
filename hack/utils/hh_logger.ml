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

(* We might want to log to both stderr and a file. Shelling out to tee isn't cross-platform.
 * We could dup2 stderr to a pipe and have a child process write to both original stderr and the
 * file, but that's kind of overkill. This is good enough *)
let dupe_log: (string * out_channel) option ref = ref None
let set_log filename fd =
  dupe_log := Some (filename, fd)
let get_log_name () = Option.map !dupe_log ~f:fst

let print_raw s =
  let time = timestamp_string () in
  begin match !dupe_log with
  | None -> ()
  | Some (_, dupe_log_oc) -> Printf.fprintf dupe_log_oc "%s %s%!" time s end;
  Printf.eprintf "%s %s%!" time s

(* wraps print_raw in order to take a format string & add a newline *)
let print fmt = Printf.ksprintf print_raw (fmt^^"\n")

let print_duration name t =
  print_raw (name ^ ": ");
  let t2 = Unix.gettimeofday() in
  Printf.eprintf "%f\n%!" (t2 -. t);
  t2

let exc ?(prefix="") e =
  print_raw (prefix ^ Printexc.to_string e ^ "\n");
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
  val passes_min_level: t -> bool
  val log : t -> ('a, unit, string, string, string, unit) format6 -> 'a
  val log_duration : t -> string -> float -> float
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

  let passes_min_level level =
    int_of_level level >= int_of_level !min_level_ref

  let log level fmt =
    if passes_min_level level
    then print fmt
    else Printf.ifprintf () fmt

  let log_duration level fmt t =
    if passes_min_level level
    then print_duration fmt t
    else t

end

(* Default log instructions to INFO level *)
let log ?(lvl=Level.Info) fmt = Level.log lvl fmt
let log_duration fmt t = Level.log_duration Level.Info fmt t

let fatal fmt = Level.log Level.Fatal fmt
let error fmt = Level.log Level.Error fmt
let warn fmt = Level.log Level.Warn fmt
let info fmt = Level.log Level.Info fmt
let debug fmt = Level.log Level.Debug fmt
