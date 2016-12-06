type saved_state_info = {
  filename : string;
  (** During server initialization, there are files that need to be
   * rechecked due to them being "dirtied" in various ways - already
   * dirtied VCS working directory, modified while parsing FASTs or
   * loading saved states, and build targetes that are not tracked. *)
  dirty_files : Relative_path.Set.t;
  changed_while_parsing : Relative_path.Set.t;
  build_targets : Relative_path.Set.t;
}

(** The debug port on the server emits a sequence of significant events. *)
type event =
  | Loaded_saved_state of saved_state_info * ServerGlobalState.t
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  (** Run a typecheck.
   * TODO: This needs a lot more stuff in it, but is not yet implemented.
   * Useful as an event for now for unit testing. *)
  | Typecheck
  (** List of files whose disk contents have changed. *)
  | Disk_files_modified of Relative_path.S.t list
  | Stop_recording

let to_string e = match e with
  | Loaded_saved_state _ -> "Loaded_saved_state"
  | Fresh_vcs_state _ -> "Fresh_vcs_state"
  | Typecheck -> "Typecheck"
  | Disk_files_modified _ -> "Disk_files_modified"
  | Stop_recording -> "Stop_recording"
