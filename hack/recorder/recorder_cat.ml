(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** The recording file contains marshaled Ocaml objects, which aren't
 * readable by non-Ocaml systems. This binary cats them to a human-readable
 * string. *)

module Args = struct

  type t = {
    recording : Path.t;
  }

  let usage = Printf.sprintf "Usage: %s [Recording path]\n" Sys.argv.(0)

  let parse () =
    let recording = ref None in
    let () = Arg.parse [] (fun s -> recording := (Some s)) usage in
    match !recording with
    | None ->
      Printf.eprintf "%s" usage;
      exit 1
    | Some recording ->
      { recording = Path.make recording; }

  let recording args = args.recording

end;;


let rec cat_recording channel =
  let event = try Marshal.from_channel channel with
  | End_of_file ->
    flush stdout;
    exit 0
  | Failure s when s = "input_value: truncated object" ->
    flush stdout;
    exit 0
  in
  Printf.printf "%s\n" (Recorder_types.to_string event);
  cat_recording channel

let () =
  Daemon.check_entry_point (); (* this call might not return *)
  let args = Args.parse () in
  let recording = Unix.openfile (Path.to_string (Args.recording args))
  [Unix.O_RDONLY]
  0o640
  in
  let recording = Unix.in_channel_of_descr recording in
  cat_recording recording
