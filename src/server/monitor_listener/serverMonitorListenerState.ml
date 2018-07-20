(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type workload = ServerEnv.env -> ServerEnv.env Lwt.t
type env_update = ServerEnv.env -> ServerEnv.env

(* Workloads are client requests which we processes FIFO *)
let workload_stream, push_new_workload = Lwt_stream.create ()
let push_new_workload workload = push_new_workload (Some workload)
(* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
 * but are quick to handle *)
let env_update_stream, push_new_env_update = Lwt_stream.create ()
let push_new_env_update env_update = push_new_env_update (Some env_update)
(* Files which have changed *)
let recheck_stream, push_files_to_recheck = Lwt_stream.create ()
let push_files_to_recheck files = push_files_to_recheck (Some files)

let pop_next_workload () =
  match Lwt_stream.get_available_up_to 1 workload_stream with
  | [ workload ] -> Some workload
  | [] -> None
  | _ -> failwith "Unreachable"

let update_env env =
  Lwt_stream.get_available env_update_stream
  |> List.fold_left (fun env f -> f env) env

let recheck_acc = ref Utils_js.FilenameSet.empty
let recheck_fetch ~process_updates =
  recheck_acc :=
    Lwt_stream.get_available recheck_stream (* Get all the files which have changed *)
    |> List.fold_left SSet.union SSet.empty (* Flatten the set *)
    |> process_updates
    |> Utils_js.FilenameSet.union (!recheck_acc) (* Union them with the acc *)
let get_and_clear_updates_for_recheck ~process_updates =
  recheck_fetch ~process_updates;
  let files = !recheck_acc in
  recheck_acc := Utils_js.FilenameSet.empty;
  files
let rec wait_for_updates_for_recheck ~process_updates =
  let%lwt _ = Lwt_stream.is_empty recheck_stream in
  recheck_fetch ~process_updates;
  if Utils_js.FilenameSet.is_empty !recheck_acc
  then wait_for_updates_for_recheck ~process_updates
  else Lwt.return_unit

(* Block until any stream receives something *)
let wait_for_anything ~process_updates =
  let%lwt () = Lwt.pick [
    (let%lwt _ = Lwt_stream.is_empty workload_stream in Lwt.return_unit);
    (let%lwt _ = Lwt_stream.is_empty env_update_stream in Lwt.return_unit);
    (let%lwt _ = Lwt_stream.is_empty recheck_stream in Lwt.return_unit);
    wait_for_updates_for_recheck ~process_updates;
  ] in
  Lwt.return_unit
