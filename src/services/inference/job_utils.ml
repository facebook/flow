(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let out_of_time ~options ~start_time =
  Unix.gettimeofday () -. start_time > Options.max_seconds_for_check_per_worker options

let out_of_memory ~options ~start_rss =
  match start_rss with
  | None -> false
  | Some start_rss ->
    (match ProcFS.status_for_pid (Unix.getpid ()) with
    | Ok { ProcFS.rss_total; _ } ->
      rss_total - start_rss > Options.max_rss_bytes_for_check_per_worker options
    | Error _ -> false)

(* Check as many files as it can before it hits the timeout. The timeout is soft,
* so the file which exceeds the timeout won't be canceled. We expect most buckets
* to not hit the timeout *)
let rec job_helper ~check ~post_check ~options ~start_time ~start_rss acc = function
  | [] -> (acc, [])
  | unfinished_files when out_of_time ~options ~start_time ->
    Hh_logger.debug
      "Bucket timed out! %d files finished, %d files unfinished"
      (List.length acc)
      (List.length unfinished_files);
    (acc, unfinished_files)
  | unfinished_files when out_of_memory ~options ~start_rss ->
    Hh_logger.debug
      "Bucket ran out of memory! %d files finished, %d files unfinished"
      (List.length acc)
      (List.length unfinished_files);
    (acc, unfinished_files)
  | file :: rest ->
    let result =
      match check file |> post_check file with
      | Ok (Some (_, acc)) -> Ok (Some acc)
      | (Ok None | Error _) as result -> result
    in
    job_helper ~check ~post_check ~options ~start_time ~start_rss ((file, result) :: acc) rest

let job ~post_check ~reader ~options acc files =
  let start_time = Unix.gettimeofday () in
  let start_rss =
    match ProcFS.status_for_pid (Unix.getpid ()) with
    | Ok { ProcFS.rss_total; _ } -> Some rss_total
    | Error _ -> None
  in
  let check = Merge_service.mk_check options ~reader () in
  job_helper ~check ~post_check ~options ~start_time ~start_rss acc files

(* A stateful (next, merge) pair. This lets us re-queue unfinished files which are returned
* when a bucket times out *)
let mk_next ~intermediate_result_callback ~max_size ~workers ~files =
  let total_count = List.length files in
  let todo = ref (files, total_count) in
  let finished_count = ref 0 in
  let num_workers = max 1 (Base.Option.value_map workers ~default:1 ~f:List.length) in
  let status_update () =
    MonitorRPC.status_update
      ServerStatus.(Checking_progress { total = Some total_count; finished = !finished_count })
  in
  let next () =
    let (remaining_files, remaining_count) = !todo in
    (* When we get near the end of the file list, start using smaller buckets in order
     * to spread the work across the available workers *)
    let bucket_size =
      if remaining_count >= max_size * num_workers then
        max_size
      else
        (remaining_count / num_workers) + 1
    in
    let (bucket, remaining_files) = Base.List.split_n remaining_files bucket_size in
    let bucket_size = List.length bucket in
    todo := (remaining_files, remaining_count - bucket_size);
    if bucket_size = 0 then
      Bucket.Done
    else
      Bucket.Job bucket
  in
  let merge (finished_file_accs, unfinished_files) acc =
    intermediate_result_callback finished_file_accs;
    let (remaining_files, remaining_count) = !todo in
    todo :=
      ( List.rev_append unfinished_files remaining_files,
        remaining_count + List.length unfinished_files );
    finished_count := !finished_count + List.length finished_file_accs;
    status_update ();
    List.rev_append finished_file_accs acc
  in
  (next, merge)
