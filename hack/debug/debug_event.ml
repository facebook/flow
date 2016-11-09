(** The debug port on the server emits a sequence of significant events. *)
type event =
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  (** Run a typecheck.
   * TODO: This needs a lot more stuff in it, but is not yet implemented.
   * Useful as an event for now for unit testing. *)
  | Typecheck
  (** List of files whose disk contents have changed. *)
  | Disk_files_modified of string list
  | Stop_recording
