(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Module used to save the state of the server. *)
(*****************************************************************************)

open Utils
open ServerEnv

type dump = ServerEnv.env * ServerArgs.options

(*****************************************************************************)
(* Primitive used to store the state of the server (checkout hh_compare.ml)
 * for the loading part.
 *)
(*****************************************************************************)

let save_in_channel filename defs oc =
  Marshal.to_channel oc defs [];
  "Saved state in: "^filename

let error_save e =
  let text = "Could not save state: " ^ (Printexc.to_string e) in
  Printf.printf "%s\n" text;
  flush stdout;
  text

let dump_state env genv =
  (* this is used mostly to debug hh build issues *)
  let data = env, genv.options in
  data

let save data filename =
  let oc = open_out filename in
  try_with_channel oc (save_in_channel filename data) error_save
