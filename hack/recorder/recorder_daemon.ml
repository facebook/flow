let marshal_events events =
  List.iter (fun e -> Marshal.to_channel stdout e []) events

let clean_exit recorder =
  let events = Recorder.get_events recorder in
  marshal_events events;
  exit 0

let rec read_and_record recorder d_in =
  let event = try Debug_port.read d_in with
    | Debug_port.Port_closed ->
      Hh_logger.log "Port closed abruptly. Flushing recording and exiting";
      clean_exit recorder
  in
  let recorder = Recorder.add_event event recorder in
  if Recorder.is_finished recorder
  then begin
    Hh_logger.log "Recording finished. Flushing recording and exiting";
    clean_exit recorder
  end else
    read_and_record recorder d_in

(** Type annotations are required here because we neve resolve them.
 * Consider actually using these phantom types.*)
let daemon_main () (ic, (_oc: unit Daemon.out_channel)) =
  Printexc.record_backtrace true;
  Hh_logger.log "Started recording";
  let d_port = Debug_port.in_port_of_in_channel ic in
  read_and_record (Recorder.start ()) d_port

let entry =
  Daemon.register_entry_point "Recorder_daemon.daemon_main" daemon_main

let maybe_rename_old_log_link log_link =
  try Sys.rename log_link (log_link ^ ".old") with _ -> ()

(** Retire the old sym link, create a new timestamped file, point the
 * link to that new file, and return a file descriptor to this file. *)
let new_file_from_link link =
  maybe_rename_old_log_link link;
  let file = Sys_utils.make_link_of_timestamped link in
  Daemon.fd_of_path file

let start_daemon output_fn log_link =
  let log_fd = new_file_from_link log_link in
  let out_fd = new_file_from_link output_fn in
  Hh_logger.log
    "About to spawn recorder daemon. Output will go to %s. Logs to %s.\n"
    output_fn log_link;
  Daemon.spawn
    (** This doesn't work in `socket mode. The recorder daemon doesn't
     * see EOF when the serve exits, and just ends up waiting forever. No
     * idea why. *)
    ~channel_mode:`pipe
    (out_fd, log_fd)
    entry
    ()
