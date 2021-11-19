(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let timestamp_string () =
  let open Unix in
  let timeofday = gettimeofday () in
  let tm = localtime timeofday in
  let ms = int_of_float (timeofday *. 1000.) mod 1000 in
  let year = tm.tm_year + 1900 in
  Printf.sprintf
    "[%d-%02d-%02d %02d:%02d:%02d.%03d]"
    year
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    ms

type dest = {
  file: bool;
  stderr: bool;
}

(* We might want to log to both stderr and a file. Shelling out to tee isn't cross-platform.
 * We could dup2 stderr to a pipe and have a child process write to both original stderr and the
 * file, but that's kind of overkill. This is good enough *)
let dupe_log : (string * out_channel) option ref = ref None

let set_log filename fd = dupe_log := Some (filename, fd)

let get_log_name () = Base.Option.map !dupe_log ~f:fst

let id : string option ref = ref None

let set_id passed_id = id := Some passed_id

let id_string () =
  match !id with
  | None -> ""
  | Some id -> Printf.sprintf "[%s] " id

let print_with_newline_internal ~dest ?exn fmt =
  let print_raw ?exn s =
    let exn_str =
      Base.Option.value_map exn ~default:"" ~f:(fun exn ->
          let bt = String_utils.indent 8 @@ String.trim @@ Exception.get_backtrace_string exn in
          let bt =
            if bt = "" then
              ""
            else
              "\n    Backtrace:\n" ^ bt
          in
          Printf.sprintf "\n    Exception: %s%s" (Exception.get_ctor_string exn) bt
      )
    in
    let time = timestamp_string () in
    let id_str = id_string () in
    begin
      match (!dupe_log, dest.file) with
      | (Some (_, dupe_log_oc), true) ->
        Printf.fprintf dupe_log_oc "%s %s%s%s\n%!" time id_str s exn_str
      | (_, _) -> ()
    end;
    if dest.stderr then Printf.eprintf "%s %s%s%s\n%!" time id_str s exn_str
  in
  Printf.ksprintf (print_raw ?exn) fmt

let print_with_newline ?exn fmt =
  print_with_newline_internal ~dest:{ file = true; stderr = true } ?exn fmt

let print_duration name t =
  let t2 = Unix.gettimeofday () in
  print_with_newline "%s: %f" name (t2 -. t);
  t2

let exc ?(prefix : string = "") ~(stack : string) (e : exn) : unit =
  print_with_newline "%s%s\n%s" prefix (Printexc.to_string e) stack

let exception_ ?(prefix : string = "") (e : Exception.t) : unit =
  exc ~prefix ~stack:(Exception.get_backtrace_string e) (Exception.unwrap e)

module Level : sig
  type t =
    | Off
    | Fatal
    | Error
    | Warn
    | Info
    | Debug

  val min_level_file : unit -> t

  val min_level_stderr : unit -> t

  val min_level : unit -> t

  val set_min_level_file : t -> unit

  val set_min_level_stderr : t -> unit

  val set_min_level : t -> unit

  val passes_min_level : t -> bool

  val log : t -> ?exn:Exception.t -> ('a, unit, string, string, string, unit) format6 -> 'a

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

  let min_level_file_ref = ref Info

  let min_level_stderr_ref = ref Info

  let min_level_file () = !min_level_file_ref

  let min_level_stderr () = !min_level_stderr_ref

  let min_level () =
    let file_int = int_of_level !min_level_file_ref in
    let stderr_int = int_of_level !min_level_stderr_ref in
    if file_int < stderr_int then
      !min_level_file_ref
    else
      !min_level_stderr_ref

  let set_min_level_file level = min_level_file_ref := level

  let set_min_level_stderr level = min_level_stderr_ref := level

  let set_min_level level =
    set_min_level_file level;
    set_min_level_stderr level;
    ()

  let passes_min_level_file level = int_of_level level >= int_of_level !min_level_file_ref

  let passes_min_level_stderr level = int_of_level level >= int_of_level !min_level_stderr_ref

  let passes_min_level level = passes_min_level_file level || passes_min_level_stderr level

  let log level ?exn fmt =
    let dest = { file = passes_min_level_file level; stderr = passes_min_level_stderr level } in
    if dest.file || dest.stderr then
      print_with_newline_internal ~dest ?exn fmt
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
