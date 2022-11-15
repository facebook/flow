(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let string_of_vcs_error vcs = function
  | Vcs_utils.Not_installed _ -> Printf.sprintf "%s is not installed" (Vcs.name vcs)
  | Vcs_utils.Errored msg -> msg

let merge_base_and_timestamp vcs vcs_root =
  let cwd = Path.to_string vcs_root in
  (* TODO: use file_watcher_mergebase_with from flowconfig *)
  let merge_base_and_timestamp =
    match vcs with
    | Vcs.Hg -> Hg.merge_base_and_timestamp ~cwd "master" "."
    | Vcs.Git -> Git.merge_base_and_timestamp ~cwd "master" "HEAD"
  in
  match%lwt merge_base_and_timestamp with
  | Error err ->
    let msg = string_of_vcs_error vcs err in
    Lwt.return_error (Printf.sprintf "Failed to compute saved state mergebase: %s" msg)
  | Ok result -> Lwt.return_ok result

let get_changes_since vcs vcs_root hash =
  let cwd = Path.to_string vcs_root in
  match vcs with
  | Vcs.Hg -> Hg.files_changed_since ~cwd hash
  | Vcs.Git -> Git.files_changed_since ~cwd hash

let saved_states options root =
  let root_str = Path.to_string root in
  let saved_states_dir =
    let saved_states_dir = Filename.concat root_str ".flow.saved_states" in
    match Options.root_name options with
    | Some name -> Filename.concat saved_states_dir (String_utils.filename_escape name)
    | None -> saved_states_dir
  in
  match Sys.readdir saved_states_dir with
  | exception Sys_error _ -> None
  | entries ->
    let regex = Str.regexp "\\([0-9]+\\)_\\([a-f0-9]+\\)" in
    let entries =
      Base.Array.filter_map entries ~f:(fun x ->
          if Str.string_match regex x 0 then
            let timestamp = Str.matched_group 1 x in
            let commit = Str.matched_group 2 x in
            match int_of_string_opt timestamp with
            | Some timestamp -> Some (timestamp, commit, x)
            | None -> None
          else
            None
      )
    in
    Some (saved_states_dir, entries)

let pick_saved_state options root merge_base timestamp =
  let ( let* ) = Option.bind in
  let* (dir, entries) = saved_states options root in
  let sort_compare (t1, c1, _) (t2, c2, _) =
    match Int.compare t1 t2 with
    | 0 ->
      (* if two saved states have the same timestamp, then we have to worry
         about whether one of them is the merge base. normally it doesn't
         really matter which one ACTUALLY comes first so we just sort the
         hashes lexographically. but if one of them is the mergebase itself,
         we want it to be the first one. below, when we binary search for
         the best saved state, we will pessimistically assume any saved state
         with the same timestamp but different hash as the mergebase is
         newer than the mergebase and can't be used. *)
      if Int.equal t1 timestamp then
        if String.equal c1 merge_base then
          -1
        else if String.equal c2 merge_base then
          1
        else
          String.compare c1 c2
      else
        String.compare c1 c2
    | k -> k
  in
  Base.Array.sort entries ~compare:sort_compare;
  let segment_of (t, c, _) =
    let k = Int.compare t timestamp in
    if k < 0 then
      `Left
    else if k > 0 then
      `Right
    else if String.equal c merge_base then
      `Left
    else
      `Right
  in
  let* i = Base.Array.binary_search_segmented entries `Last_on_left ~segment_of in
  (* TODO: use timestamp to enforce a max age. it can take longer to load
     from an extremely stale saved state and then incrementally handle a ton
     of changes. *)
  let (_timestamp, hash, filename) = Array.get entries i in
  Some (hash, Filename.concat dir filename)

(** This saved state fetcher loads saved states associated with source control
  revisions. It selects the most recent saved state based on the public commit
  hash. The saved states are stored in the .flow.saved_states folder in the
  project root directory and are indexed by timestamp and commit hash.

    /path/to/root/.flow.saved_states/1666400734_faceb00c
    /path/to/root/.flow.saved_states/1666400738_deadbeef

  If the public ancestor (e.g. the mergebase with the main branch) of the
  currently checked-out commit was committed at 1666400736, then the most
  recent saved state is 1666400734_faceb00c. Source control will be queried
  for changes since commit faceb00c, instead of needing a "files_changed"
  file like the "local" fetcher requires.

  If multiple commits have the same timestamp, the chosen one is stable but
  unspecified (currently, it's the first one lexographically, but this is
  subject to change). However, if one of them is the current mergebase, it
  is selected.

  If the [name] option is set in .flowconfig, then saved states should
  be stored in a correspondingly-named subdirectory of .flow.saved_states.
  If [name=foo], then saved states should be stored in
  /path/to/root/.flow.saved_states/foo/TIMESTAMP_HASH

  How to distribute saved states to clients is left as an exercise for the
  reader. But the benefit of this fetcher compared to the "local" fetcher
  is that multiple states can be prepopulated. If you want to be fancy,
  you could even imagine a virtual file system that fetches the states
  from a remote source on demand. *)
let fetch ~options =
  Profiling_js.with_profiling_lwt ~label:"FetchSavedState" ~should_print_summary:false (fun _ ->
      let root = Options.root options in
      match Vcs.find_root root with
      | None ->
        Hh_logger.error "Unable to detect a source control root: no .git or .hg folder";
        Lwt.return Saved_state_fetcher.Saved_state_error
      | Some (vcs, vcs_root) ->
        (match%lwt merge_base_and_timestamp vcs vcs_root with
        | Error msg ->
          Hh_logger.error "%s" msg;
          Lwt.return Saved_state_fetcher.Saved_state_error
        | Ok (merge_base, timestamp) ->
          Hh_logger.info "Saved state merge base hash is %S" merge_base;
          (match pick_saved_state options root merge_base timestamp with
          | None -> Lwt.return Saved_state_fetcher.No_saved_state
          | Some (saved_state_merge_base, saved_state_file) ->
            (match%lwt get_changes_since vcs vcs_root saved_state_merge_base with
            | Ok changed_files ->
              let saved_state_filename = Path.make saved_state_file in
              let changed_files = SSet.of_list changed_files in
              let changed_files_count = SSet.cardinal changed_files in
              Hh_logger.info "Saved state path is %s" saved_state_file;
              Hh_logger.info "%d files changed since saved state was created" changed_files_count;
              Lwt.return (Saved_state_fetcher.Saved_state { saved_state_filename; changed_files })
            | Error err ->
              let msg = string_of_vcs_error vcs err in
              Hh_logger.error "Failed to fetch files changed since the saved state: %s" msg;
              Lwt.return Saved_state_fetcher.Saved_state_error)))
  )
