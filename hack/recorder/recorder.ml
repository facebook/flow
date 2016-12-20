(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

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

let start init_env =
  let start = Unix.gettimeofday () in
  let buffer = [Start_recording init_env] in
  Active ({ start_time = start; rev_buffered_recording = buffer; })

let default_instance = Switched_off

(** Use fixed buffer to minimize heap allocations. *)
let chunk_size = 65536
let read_buffer = String.create chunk_size

exception File_read_error

let rec fetch_file_contents_batched acc fd =
  let bytes_read = Unix.read fd read_buffer 0 chunk_size in
  if bytes_read = 0 then
    acc
  else if bytes_read < 0 then
    raise File_read_error
  else
    let b = String.sub read_buffer 0 bytes_read in
    fetch_file_contents_batched (b :: acc) fd

let with_opened_file path f =
  let fd = Unix.openfile path [Unix.O_RDONLY] 0o440 in
  let f () = f fd in
  let finally () = Unix.close fd in
  Utils.try_finally ~f ~finally

let fetch_file_contents path =
  let path = Relative_path.to_absolute path in
  let buffers_rev = with_opened_file path @@ fetch_file_contents_batched [] in
  String.concat "" @@ List.rev buffers_rev

let fetch_file_contents_opt path =
  try Some (fetch_file_contents path) with
  | Unix.Unix_error(Unix.ENOENT, _, _) -> None

let files_with_contents files =
  let files = Relative_path.Set.elements files in
  Relative_path.Map.from_keys files fetch_file_contents

(** Like files_with_contents but skips over non-existent files. *)
let files_with_contents_opt files =
  let files = Relative_path.Set.elements files in
  let contents = Relative_path.Map.from_keys files @@ fetch_file_contents_opt in
  Relative_path.Map.fold contents ~init:Relative_path.Map.empty
    ~f:(fun key data acc ->
      match data with
      | None -> acc
      | Some data -> Relative_path.Map.add acc ~key ~data
    )

let convert_event debug_event = match debug_event with
  | DE.Loaded_saved_state (
    { DE.filename; dirty_files; changed_while_parsing; build_targets; },
    global_state) ->
    let dirty_files = files_with_contents dirty_files in
    let changed_while_parsing = files_with_contents changed_while_parsing in
    (** TODO: Some build target files might not exist. Skip them.
     * Improve this. *)
    let build_targets = files_with_contents_opt build_targets in
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
