type saved_state_info = {
  filename : string;
  dirty_files : string Relative_path.Map.t;
  changed_while_parsing : string Relative_path.Map.t;
  build_targets : string Relative_path.Map.t;
}

(** We need these paths to be restored in all the process that handle
 * Relative_path.t - the recorder daemon, the turntable, etc. *)
type init_env = {
  root_path : Path.t;
  hhi_path : Path.t;
}

(** A recording is a sequence of these events. They are derived from the events
 * emitted from the debug port.*)
type event =
  | Start_recording of init_env
  | Loaded_saved_state of saved_state_info * ServerGlobalState.t
  (** The state name of the fresh VCS state. *)
  | Fresh_vcs_state of string
  | Typecheck
  | HandleServerCommand : 'a ServerCommandTypes.command -> event
  (** Files whose disk contents have changed and their new contents. *)
  | Disk_files_modified of string Relative_path.Map.t
  | Stop_recording

let to_string e = match e with
  | Start_recording _ -> "Start_recording"
  | Loaded_saved_state ({ filename; dirty_files; _ }, _) ->
    Printf.sprintf "(Loaded_saved_state %s with %d dirtied files)"
      filename (Relative_path.Map.cardinal dirty_files)
  | Fresh_vcs_state s ->
    Printf.sprintf "(Fresh_vcs_state %s)" s
  | Typecheck -> "Typecheck"
  | HandleServerCommand cmd ->
    Printf.sprintf "(HandleServerCommand %s)"
      (ServerCommandTypesUtils.debug_describe_cmd cmd)
  | Disk_files_modified files ->
    Printf.sprintf "Files_modified: [%s ]"
    (List.fold_left (
      fun acc path -> acc ^ " " ^ (Relative_path.S.to_string path)
      ) "" (Relative_path.Map.keys files))
  | Stop_recording -> "Stop_recording"
