type saved_state_info = {
  filename : string;
  dirty_files : string Relative_path.Map.t;
  changed_while_parsing : string Relative_path.Map.t;
  build_targets : string Relative_path.Map.t;
}


(** A recording is a sequence of these events. They are derived from the events
 * emitted from the debug port.*)
type event =
  | Loaded_saved_state of saved_state_info * ServerGlobalState.t
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  | Typecheck
  (** Files whose disk contents have changed and their new contents. *)
  | Disk_files_modified of string Relative_path.Map.t
  | Stop_recording

let to_string e = match e with
  | Loaded_saved_state ({ filename; dirty_files; _ }, _) ->
    Printf.sprintf "(Loaded_saved_state %s with %d dirtied files)"
      filename (Relative_path.Map.cardinal dirty_files)
  | Fresh_vcs_state s ->
    Printf.sprintf "(Fresh_vcs_state %s)" s
  | Typecheck -> "Typecheck"
  | Disk_files_modified files ->
    Printf.sprintf "Files_modified: [%s ]"
    (List.fold_left (
      fun acc path -> acc ^ " " ^ (Relative_path.S.to_string path)
      ) "" (Relative_path.Map.keys files))
  | Stop_recording -> "Stop_recording"
