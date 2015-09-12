(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a in_channel = Pervasives.in_channel
type 'a out_channel = Pervasives.out_channel

type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel

type ('in_, 'out) handle = {
  channels : ('in_, 'out) channel_pair;
  pid : int;
}

let to_channel : 'a out_channel -> ?flush:bool -> 'a -> unit =
fun oc ?flush:(should_flush=true) v ->
  Marshal.to_channel oc v [];
  if should_flush then flush oc

let from_channel : 'a in_channel -> 'a = fun ic ->
  Marshal.from_channel ic

let flush : 'a out_channel -> unit = Pervasives.flush

let descr_of_in_channel : 'a in_channel -> Unix.file_descr =
  Unix.descr_of_in_channel

let descr_of_out_channel : 'a out_channel -> Unix.file_descr =
  Unix.descr_of_out_channel

let make_pipe () =
  let descr_in, descr_out = Unix.pipe () in
  (* close descriptors on exec so they are not leaked *)
  Unix.set_close_on_exec descr_in;
  Unix.set_close_on_exec descr_out;
  let ic = Unix.in_channel_of_descr descr_in in
  let oc = Unix.out_channel_of_descr descr_out in
  ic, oc

let fork ?log_name (f : ('a, 'b) channel_pair -> unit) :
    ('b, 'a) handle =
  let parent_in, child_out = make_pipe () in
  let child_in, parent_out = make_pipe () in
  match Fork.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      close_in parent_in;
      close_out parent_out;
      let fd = Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CREAT] 0o777 in
      Unix.dup2 fd Unix.stdin;
      Unix.close fd;
      let fn = Option.value_map log_name ~default:"/dev/null" ~f:
        begin fun fn ->
          let fn = Printf.sprintf "%s/%s.daemon" (Tmp.get_dir ()) fn in
          begin try Sys.rename fn (fn ^ ".old") with _ -> () end;
          fn
        end in
      let fd = Unix.openfile fn [Unix.O_WRONLY; Unix.O_CREAT] 0o666 in
      Unix.dup2 fd Unix.stdout;
      Unix.dup2 fd Unix.stderr;
      Unix.close fd;
      f (child_in, child_out);
      exit 0
  | pid -> (* parent *)
      close_in child_in;
      close_out child_out;
      { channels = parent_in, parent_out; pid }

(* for testing code *)
let devnull () =
  let ic = open_in "/dev/null" in
  let oc = open_out "/dev/null" in
  {channels = ic, oc; pid = 0}
