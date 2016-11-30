(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(** This binary connects to the debug port on a hack server and sends the
 * stream of events to the Recorder to create a valid recording.
 *
 * Note: Currently, it only supports starting a new Hack server instance, and
 * fails if one is already running (primarily for simplicity of an initial
 * implementation - connect to debug port by creating a pipe then forking
 * the server process). This will be improved in the future to add the ability
 * to attach to an existing server process. *)
(*****************************************************************************)

module DE = Debug_event

module Globals = struct

  (**
   * We need to have a global reference here to support asynchronous
   * interruptions from Unix signals.
   *
   * This makes me sad too, but other approaches are essentially equivalent
   * to this. *)
  let recorder = ref Recorder.default_instance
end

module Args = struct

  type t = {
    root : Path.t;
  }

  let usage = Printf.sprintf "Usage: %s [WWW DIRECTORY]\n" Sys.argv.(0)

  let parse () =
    let root = ref None in
    let () = Arg.parse [] (fun s -> root := (Some s)) usage in
    let root = ClientArgsUtils.get_root !root in
    {
      root = root;
    }

  let root args = args.root

end

let oprint_events events =
  List.iter (fun e -> Printf.printf "%s\n" (Recorder_types.to_string e)) events

let print_and_exit recorder =
  let events = Recorder.get_events recorder in
  oprint_events events;
  exit 0

let rec read_and_record recorder (d_in: Debug_port.in_port) =
  let event = Debug_port.read d_in in
  let () = recorder := Recorder.add_event event !recorder in
  if Recorder.is_finished !recorder
  then
    print_and_exit !recorder
  else
    read_and_record recorder d_in

let stop_recording () =
  Globals.recorder := (Recorder.add_event DE.Stop_recording !Globals.recorder);
  print_and_exit !Globals.recorder

let () =
  let () = Globals.recorder := Recorder.start () in
  (** Stop the recording when we get a Unix interrupt signal. *)
  Sys_utils.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    let () = stop_recording () in
    raise Exit_status.(Exit_with Interrupted)));
  let args = Args.parse () in
  HackEventLogger.client_init @@ Args.root args;
  let d_in, d_out = Debug_port.create () in
  let start_env = {
    ClientStart.root = args.Args.root;
    ai_mode = None;
    no_load = false;
    silent = false;
    debug_port = (Some d_out);
  } in
  let e = ClientStart.main start_env in
  match e with
  | Exit_status.No_error ->
    let () = Printf.eprintf "Server started with debug port attached.\n" in
    let () = Printf.eprintf "Starting recording.\n" in
    read_and_record Globals.recorder d_in
  | _ ->
    Exit_status.(exit e)
