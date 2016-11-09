(** A recording is a sequence of these events. They are derived from the events
 * emitted from the debug port.*)
type event =
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  | Typecheck
  (** Files whose disk contents have changed and their new contents. *)
  | Disk_files_modified of string SMap.t
  | Stop_recording

let to_string e = match e with
  | Fresh_vcs_state s ->
    Printf.sprintf "(Fresh_vcs_state %s)" s
  | Typecheck -> "Typecheck"
  | Disk_files_modified _ -> "Files_modified"
  | Stop_recording -> "Stop_recording"
