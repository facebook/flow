(**
 *  Copyright 2015 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

type 'a in_channel = Pervasives.in_channel
type 'a out_channel = Pervasives.out_channel

type ('in_, 'out) handle = 'in_ in_channel * 'out out_channel

let to_channel : 'a out_channel -> 'a -> unit = fun oc v ->
  Marshal.to_channel oc v []

let from_channel : 'a in_channel -> 'a = fun ic ->
  Marshal.from_channel ic

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

let fork (f : ('a, 'b) handle -> unit) : ('b, 'a) handle =
  let parent_in, child_out = make_pipe () in
  let child_in, parent_out = make_pipe () in
  match Unix.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      close_in parent_in;
      close_out parent_out;
      f (child_in, child_out);
      exit 0
  | pid -> (* parent *)
      close_in child_in;
      close_out child_out;
      parent_in, parent_out
