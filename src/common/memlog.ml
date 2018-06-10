(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let root_r = ref ""
let count_r = ref 1

let set_root (root: Path.t) : unit =
  root_r := Path.to_string root;
  Hh_logger.Level.set_min_level Hh_logger.Level.Debug


let stack () : string =
  let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
  let stack = String.split_on_char '\n' stack in

  let regexp = Str.regexp ".*\\(lwt\\|lwt_sequence\\|lwt_engine\\|lwt_main\\|memlog\\).ml.*" in
  let stack = List.filter (fun line -> not (Str.string_match regexp line 0)) stack in
  let regex = Str.regexp "\"[^\"]*\\(/\\|\\\\\\)flow\\(/\\|\\\\\\)src" in
  let stack = List.map (fun line -> Str.replace_first regex "\"src" line) stack in
  let regex = Str.regexp "\"[^\"]*\\(/\\|\\\\\\)hphp\\(/\\|\\\\\\)hack\\(/\\|\\\\\\)src" in
  let stack = List.map (fun line -> Str.replace_first regex "\"hack/src" line) stack in
  let regex = Str.regexp "Raised by primitive operation at " in
  let stack = List.map (fun line -> Str.replace_first regex "" line) stack in
  let regex = Str.regexp "Called from " in
  let stack = List.map (fun line -> Str.replace_first regex "" line) stack in
  let regex = Str.regexp ("file \"\\([^\"]*\\)\"\\([^,]*\\), "
    ^ "line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)") in
  let stack = List.map (fun line -> Str.replace_first regex "\\1:\\3.\\4-\\5\\2" line) stack in

  let stack = List.map (fun line -> "   " ^ line) stack in
  let stack = String.concat "\n" stack in
  stack


let log_acc (message: string) : unit =
  let message = Printf.sprintf "[%s]Memlog: %s\n" (Hh_logger.timestamp_string ()) message in
  Hh_logger.acc_r := !Hh_logger.acc_r ^ message

let log_disk (message: string) : unit =
  if !root_r = "" then () else begin
  let nfn = Filename.concat !root_r "ljw.log" in
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 nfn in
  let s = Printf.sprintf "**** [%s] %s\n" (Hh_logger.timestamp_string ()) message in
  output_string oc s;
  close_out oc
  end

let log (message: string) : unit =
  if !count_r = 0 then log_disk message else log_acc message

let suspend () : unit =
  if !count_r = 0 then Hh_logger.acc_r := "";
  count_r := !count_r + 1

let resume () : unit =
  count_r := !count_r - 1;
  if !count_r = 0 then log_disk (">>>>>>>>ACC\n" ^ !Hh_logger.acc_r ^ "<<<<<<<<<<<ACC\n\n")

let flush () : unit =
  if !count_r = 0 then () else
  count_r := 0;
  log_disk (">>>>>>>>ACC[exit]\n" ^ !Hh_logger.acc_r ^ "<<<<<<<<<<<ACC[exit]\n\n")
