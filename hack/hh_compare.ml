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
(* Tool used to compare two dumps of the state of the server 
 * (cf serverSign.ml).
 * When one calls "hh_client --save my_state" it saves the state of the
 * server.
 * The idea is to go back and forth between revisions with complex operations
 * and check the consistency of the state.
 * This tool compares two states and reports the difference.
 *)
(*****************************************************************************)


open Utils

(*****************************************************************************)
(* Loading from file *)
(*****************************************************************************)

let load filename =
  let ic = open_in filename in
  let funs, classes = Marshal.from_channel ic in
  close_in ic;
  funs, classes

(*****************************************************************************)
(* Comparison functions *)
(*****************************************************************************)

let compare_map_left visited m1 m2 =
  let diff = ref [] in
  SMap.iter begin fun name value1 ->
    if SSet.mem name !visited then () else
    begin
      visited := SSet.add name !visited;
      match SMap.get name m2 with
      | None ->
          diff := (name, value1, None) :: !diff
      | Some value2 ->
          if value1 <> value2
          then diff := (name, value1, value2) :: !diff
          else ()
    end
  end m1;
  !diff

let compare_map m1 m2 =
  let visited = ref SSet.empty in
  let cmp x y = compare_map_left visited x y in
  cmp m1 m2 @ cmp m2 m1

let compare_states (funs1, classes1) (funs2, classes2) =
  let diff_funs = compare_map funs1 funs2 in
  let diff_classes = compare_map classes1 classes2 in
  let opt_str = function None -> "none" | Some x -> x in
  List.iter begin fun (name, v1, v2) ->
    let v1 = opt_str v1 in
    let v2 = opt_str v2 in
    Printf.printf "Found difference: %s\n%s\n%s\n\n" name v1 v2
  end (diff_funs @ diff_classes)

(*****************************************************************************)
(* Main *)
(*****************************************************************************)

let () =
  let filename1 = Sys.argv.(1) in
  let filename2 = Sys.argv.(2) in
  let state1 = load filename1 in
  let state2 = load filename2 in
  compare_states state1 state2
