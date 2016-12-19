open Recorder_types

module DE = Debug_event

(** Recorder handles the stream of debug events from hack to
 * create a valid recording. It does things like ignoring events
 * until a valid recording can begin, catting the actual contents
 * of files from disk, attempting to detect when a recording goes bad,
 * and identifying and summarizing version control checkout changes
 * (to avoid catting file contents between checkouts). *)

let file_extension = "hrec"

type env = {
  start_time : float;
  (** Reversed list of events. i.e., the most-recent event is first in the
   * list. *)
  rev_buffered_recording: event list;
}

type instance =
  | Switched_off
  | Active of env
  | Finished of event list

let is_finished instance = match instance with
  | Finished _ -> true
  | _ -> false

let get_events instance = match instance with
  | Finished events -> events
  | Switched_off -> []
  | Active env -> List.rev env.rev_buffered_recording

let start () =
  let start = Unix.gettimeofday () in
  Active ({ start_time = start; rev_buffered_recording = []; })

let default_instance = Switched_off

let fetch_file_contents _ =
  (** TODO: Get the contents from disk *)
  "Lorem ipsum...\n"

let files_with_contents files =
  let files = Relative_path.Set.elements files in
  Relative_path.Map.from_keys files fetch_file_contents

let convert_event debug_event = match debug_event with
  | DE.Loaded_saved_state (
    { DE.filename; dirty_files; changed_while_parsing; build_targets; },
    global_state) ->
    let dirty_files = files_with_contents dirty_files in
    let changed_while_parsing = files_with_contents changed_while_parsing in
    let build_targets = files_with_contents build_targets in
    Loaded_saved_state (
      { filename;
      dirty_files;
      changed_while_parsing;
      build_targets; },
      global_state)
  | DE.Fresh_vcs_state s -> Fresh_vcs_state s
  | DE.Typecheck -> Typecheck
  | DE.HandleServerCommand cmd -> HandleServerCommand cmd
  | DE.Disk_files_modified files ->
    let contents = Relative_path.Map.from_keys files fetch_file_contents in
    Disk_files_modified contents
  | DE.Stop_recording ->
    Stop_recording

let with_event event env =
  { env with rev_buffered_recording =
    (convert_event event) :: env.rev_buffered_recording; }

let add_event event instance = match instance, event with
  | Finished _, _ ->
    instance
  | Switched_off, _ ->
    instance
  | Active env, DE.Stop_recording ->
    Finished (List.rev env.rev_buffered_recording)
  | Active env, _ ->
    let env = with_event event env in
    Active env
