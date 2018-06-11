(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let root_r = ref ""
let acc_r = ref ""

let set_root (root: string) : unit =
  root_r := root

let time () : string =
  let open Unix in
  let f = gettimeofday () in
  let (msec, _sec) = modf f in
  let tm = localtime f in
  Printf.sprintf "%04d.%02d.%02d %02d:%02d:%02d.%03d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
    (int_of_float (msec *. 1000.))

let stack () : string =
  let stack = Printexc.get_callstack 100 |> Printexc.raw_backtrace_to_string in
  let stack = String.split_on_char '\n' stack in

  let regexp = Str.regexp ".*\\(lwt\\|lwt_sequence\\|lwt_engine\\|lwt_main\\).ml.*" in
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


let acc (s: string) : unit =
  let s = Printf.sprintf "[%s] %s\n%s\n\n" (time ()) s (stack ()) in
  acc_r := !acc_r ^ s

let clear () : unit =
  acc_r := ""

let get () : string =
  let r = !acc_r in
  clear ();
  r

let log (message: string) : unit =
  if !root_r = "" then failwith "must set root";
  let nfn = Filename.concat !root_r "ljw.log" in
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 nfn in
  let s = Printf.sprintf "**** [%s] %s\n%s\n\n" (time ()) message (stack ()) in
  output_string oc s;
  close_out oc

let log_acc () : unit =
  log (get ())
