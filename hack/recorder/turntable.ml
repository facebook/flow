(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Turntable plays back a recording on a Hack server. Upon finishing playback,
 * it sleeps until manually terminated, keeping the persistent connection
 * alive so that the server state doesn't drop the IDE state (allowing you to
 * to interact with and inspect the server).
 *)

module Args = struct

  type t = {
    root : Path.t;
    recording : Path.t;
  }

  let usage = Printf.sprintf
    "Usage: %s --recording <recording file> [REPO DIRECTORY]\n"
    Sys.argv.(0)

  let parse () =
    let root = ref None in
    let recording = ref "" in
    let options = [
      "--recording", Arg.String (fun x -> recording := x),
      "Path to the recording file";
    ] in
    let () = Arg.parse options (fun s -> root := (Some s)) usage in
    let root = ClientArgsUtils.get_root !root in
    if !recording = "" then
      let () = Printf.eprintf "%s" usage in
      exit 1
    else
    {
      root = root;
      recording = Path.make !recording;
    }

  let root args = args.root

end;;

let not_yet_supported s =
  Printf.eprintf "Type not yet supported: %s" s

let playback_server_cmd : type a. Timeout.in_channel * out_channel ->
  a ServerCommandTypes.command -> unit = fun conn cmd ->
  match cmd with
  | ServerCommandTypes.Rpc rpc ->
    let _ = ClientIde.rpc conn rpc in
    ()
  | ServerCommandTypes.Stream _ ->
    not_yet_supported "ServerCommandTypes.Stream"
  | ServerCommandTypes.Debug ->
    not_yet_supported "ServerCommandTypes.Debug"

let rec playback recording conn =
  let x = Marshal.from_channel recording in
  let open Recorder_types in
  let () = match x with
  | Loaded_saved_state _ ->
    (** TODO *)
    playback recording conn
  | Fresh_vcs_state _ ->
    not_yet_supported "Fresh_vcs_state"
  | Typecheck ->
    not_yet_supported "Typecheck"
  | HandleServerCommand cmd ->
    playback_server_cmd conn cmd
  | Disk_files_modified _ ->
    not_yet_supported "Disk_files_modified"
  | Stop_recording ->
    not_yet_supported "Stop_recording"
  in
  playback recording conn

let rec sleep_and_wait () =
  let _, _, _ = Unix.select [] [] [] 999999.999 in
  sleep_and_wait ()

let () =
  Daemon.check_entry_point (); (* this call might not return *)
  Sys_utils.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    exit 0));
  let args = Args.parse () in
  HackEventLogger.client_init @@ Args.root args;
  let conn = ClientIde.connect_persistent { ClientIde.root = Args.root args }
    ~retries:800 in
  let recording = args.Args.recording |> Path.to_string |> open_in in
  try playback recording conn with
  | End_of_file ->
    Printf.eprintf "End of recording...waiting for termination\n%!";
    sleep_and_wait ()
  | Failure s when s = "input_value: truncated object" ->
    Printf.eprintf "End of recording...waiting for termination\n%!";
    sleep_and_wait ()
