(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let with_in_channel filename f =
  let ic = open_in_bin filename in
  try let res = f ic in close_in ic; res
  with exn -> close_in ic; raise exn

let with_out_channel filename f =
  let oc = open_out_bin filename in
  try let res = f oc in close_out oc; res
  with exn -> close_out oc; raise exn

(* Read the first line in stdout or stderr of an external command. *)
let read_process_output cmd =
  let channels = Unix.open_process_full cmd (Unix.environment ()) in
  let (out_c, _, err_c) = channels in
  let out = try Some (input_line out_c) with | End_of_file -> None in
  let err = try Some (input_line err_c) with | End_of_file -> None in
  let msg = match out, err with
    | Some o, Some e -> o ^ "\n" ^ e
    | Some o, None -> o
    | None, Some e -> e
    | _ -> ""
  in
  match Unix.close_process_full channels with
  | Unix.WEXITED 0 -> msg
  | _ -> raise (Failure msg)

let string_of_file filename =
  with_in_channel filename @@ fun ic ->
  let s = String.create 32759 in
  let b = Buffer.create 1000 in
  let rec iter ic b s =
    let nread = input ic s 0 32759 in
    if nread > 0 then begin
      Buffer.add_substring b s 0 nread;
      iter ic b s
    end in
  iter ic b s;
  Buffer.contents b
