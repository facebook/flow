(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external realpath: string -> string option = "hh_realpath"

let open_in_no_fail fn = 
  try open_in fn
  with e -> 
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_in: '%s' (%s)\n" fn e;
    exit 3

let close_in_no_fail fn ic =
  try close_in ic with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let open_out_no_fail fn =
  try open_out fn
  with e -> 
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_out: '%s' (%s)\n" fn e;
    exit 3

let close_out_no_fail fn oc =
  try close_out oc with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let cat filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in ic;
  content

let cat_no_fail filename =
  let ic = open_in_no_fail filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in_no_fail filename ic;
  content

let nl_regexp = Str.regexp "[\r\n]"
let split_lines = Str.split nl_regexp

let exec_read cmd =
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  result

let restart () =
  let cmd = Sys.argv.(0) in
  let argv = Sys.argv in
  Unix.execv cmd argv

let logname_impl () =
  try Sys.getenv "USER" with Not_found ->
  try Sys.getenv "LOGNAME" with Not_found ->
  (* If this function is generally useful, it can be lifted to toplevel in this
   * file, but this is the only place we need it for now. *)
  let exec_try_read cmd =
    let ic = Unix.open_process_in cmd in
    let out = try Some (input_line ic) with End_of_file -> None in
    let status = Unix.close_process_in ic in
    match out, status with
      | Some _, Unix.WEXITED 0 -> out
      | _ -> None
  in
  try Utils.unsafe_opt (exec_try_read "logname") with Invalid_argument _ ->
  try Utils.unsafe_opt (exec_try_read "id -un") with Invalid_argument _ ->
  "[unknown]"

let logname_ref = ref None
let logname () =
  if !logname_ref = None then logname_ref := Some (logname_impl ());
  Utils.unsafe_opt !logname_ref

let with_umask umask f =
  let old_umask = ref 0 in
  Utils.with_context
    ~enter:(fun () -> old_umask := Unix.umask umask)
    ~exit:(fun () -> Unix.umask !old_umask)
    ~do_:f

let with_timeout timeout ~on_timeout ~do_ =
  let old_handler = ref Sys.Signal_default in
  let old_timeout = ref 0 in
  Utils.with_context
    ~enter:(fun () ->
      old_handler := Sys.signal Sys.sigalrm (Sys.Signal_handle on_timeout);
      old_timeout := Unix.alarm timeout)
    ~exit:(fun () ->
      ignore (Unix.alarm !old_timeout);
      Sys.set_signal Sys.sigalrm !old_handler)
    ~do_

let read_stdin_to_string () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf
