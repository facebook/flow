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

let fork (f : ('a, 'b) channel_pair -> unit) : ('b, 'a) handle =
  let parent_in, child_out = make_pipe () in
  let child_in, parent_out = make_pipe () in
  match Fork.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      close_in parent_in;
      close_out parent_out;
      f (child_in, child_out);
      exit 0
  | pid -> (* parent *)
      close_in child_in;
      close_out child_out;
      { channels = parent_in, parent_out; pid }
