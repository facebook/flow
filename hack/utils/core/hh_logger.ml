(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

let timestamp_string () =
  Unix.(
    let tm = localtime (time ()) in
    let ms = int_of_float (gettimeofday () *. 1000.) mod 1000 in
    let year = tm.tm_year + 1900 in
    Printf.sprintf
      "[%d-%02d-%02d %02d:%02d:%02d.%03d]"
      year
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
      ms)

(* We might want to log to both stderr and a file. Shelling out to tee isn't cross-platform.
 * We could dup2 stderr to a pipe and have a child process write to both original stderr and the
 * file, but that's kind of overkill. This is good enough *)
let dupe_log : (string * out_channel) option ref = ref None

let set_log filename fd = dupe_log := Some (filename, fd)

let get_log_name () = Option.map !dupe_log ~f:fst

let print_with_newline ?exn fmt =
  let print_raw ?exn s =
    let exn_str =
      Option.value_map exn ~default:"" ~f:(fun exn ->
          let bt =
            String_utils.indent 8
            @@ String.trim
            @@ Exception.get_backtrace_string exn
          in
          let bt =
            if bt = "" then
              ""
            else
              "\n    Backtrace:\n" ^ bt
          in
          Printf.sprintf
            "\n    Exception: %s%s"
            (Exception.get_ctor_string exn)
            bt)
    in
    let time = timestamp_string () in
    begin
      match !dupe_log with
      | None -> ()
      | Some (_, dupe_log_oc) ->
        Printf.fprintf dupe_log_oc "%s %s%s\n%!" time s exn_str
    end;
    Printf.eprintf "%s %s%s\n%!" time s exn_str
  in
  Printf.ksprintf (print_raw ?exn) fmt

let print_duration name t =
  let t2 = Unix.gettimeofday () in
  print_with_newline "%s: %f" name (t2 -. t);
  t2

let exc ?(prefix : string = "") ~(stack : string) (e : exn) : unit =
  print_with_newline "%s%s\n%s" prefix (Printexc.to_string e) stack

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

  val passes_min_level : t -> bool

  val log :
    t ->
    ?exn:Exception.t ->
    ('a, unit, string, string, string, unit) format6 ->
    'a

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

  let log level ?exn fmt =
    if passes_min_level level then
      print_with_newline ?exn fmt
    else
      Printf.ifprintf () fmt

  let log_duration level fmt t =
    if passes_min_level level then
      print_duration fmt t
    else
      t
end

(* Default log instructions to INFO level *)
let log ?(lvl = Level.Info) fmt = Level.log lvl fmt

let log_duration fmt t = Level.log_duration Level.Info fmt t

let fatal ?exn fmt = Level.log Level.Fatal ?exn fmt

let error ?exn fmt = Level.log Level.Error ?exn fmt

let warn ?exn fmt = Level.log Level.Warn ?exn fmt

let info ?exn fmt = Level.log Level.Info ?exn fmt

let debug ?exn fmt = Level.log Level.Debug ?exn fmt
