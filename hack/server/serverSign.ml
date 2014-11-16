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
(* Extracts the definition names. *)
(*****************************************************************************)

let add_name acc (_, x) = SSet.add x acc

let get_defs file_map =
  SMap.fold begin fun _filename
    {FileInfo.funs; classes; _}
      (all_funs, all_classes) ->
    List.fold_left add_name all_funs funs,
    List.fold_left add_name all_classes classes
  end file_map (SSet.empty, SSet.empty)

(*****************************************************************************)
(* Performs a checksum. For function that's all we do, keeping the entire
 * type definition is not necessary. That's because the type of a function
 * doesn't depend on external types.
 *)
(*****************************************************************************)

let checksum v =
  let v = Marshal.to_string v [] in
  let v = Digest.string v in
  let v = Digest.to_hex v in
  v

let sum_fun = opt_map checksum

(* In case of a class, we dump the entire type. *)
let sum_class = opt_map Typing_print.class_

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

(* old: was working with hh_compare
  let funs, classes = get_defs env.ServerEnv.files_info in
  let funs = Typing_env.Funs.get_batch funs in
  let classes = Typing_env.Classes.get_batch classes in
  let funs = SMap.map sum_fun funs in
  let classes = SMap.map sum_class classes in
  let defs = funs, classes in
  let oc = open_out filename in
  try_with_channel oc (save_in_channel filename defs) error_save
*)
