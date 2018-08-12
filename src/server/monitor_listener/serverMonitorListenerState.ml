(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameSet = Utils_js.FilenameSet

type workload = ServerEnv.env -> ServerEnv.env Lwt.t
type env_update = ServerEnv.env -> ServerEnv.env

(* Workloads are client requests which we processes FIFO *)
let workload_stream, push_new_workload = Lwt_stream.create ()
let push_new_workload workload = push_new_workload (Some workload)
(* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
 * but are quick to handle *)
let env_update_stream, push_new_env_update = Lwt_stream.create ()
let push_new_env_update env_update = push_new_env_update (Some env_update)

type recheck_msg = {
  files: SSet.t;
  callback: (Profiling_js.finished option -> unit) option;
  focus: bool;
}
(* Files which have changed *)
let recheck_stream, push_recheck_msg = Lwt_stream.create ()
let push_recheck_msg ~focus ?callback files = push_recheck_msg (Some { files; callback; focus; })
let push_files_to_recheck = push_recheck_msg ~focus:false
let push_files_to_focus = push_recheck_msg ~focus:true

let pop_next_workload () =
  match Lwt_stream.get_available_up_to 1 workload_stream with
  | [ workload ] -> Some workload
  | [] -> None
  | _ -> failwith "Unreachable"

let update_env env =
  Lwt_stream.get_available env_update_stream
  |> List.fold_left (fun env f -> f env) env

type recheck_workload = {
  files_to_recheck: FilenameSet.t;
  files_to_focus: FilenameSet.t;
  profiling_callbacks: (Profiling_js.finished option -> unit) list;
}
let empty_recheck_workload = {
  files_to_recheck = FilenameSet.empty;
  files_to_focus = FilenameSet.empty;
  profiling_callbacks = [];
}

let recheck_workload_is_empty { files_to_recheck; files_to_focus; profiling_callbacks=_; } =
  FilenameSet.is_empty files_to_recheck && FilenameSet.is_empty files_to_focus
let recheck_acc = ref empty_recheck_workload
let recheck_fetch ~process_updates =
  recheck_acc :=
    Lwt_stream.get_available recheck_stream (* Get all the files which have changed *)
    |> Core_list.fold_left ~init:(!recheck_acc) ~f:(fun workload { files; callback; focus; } ->
        let files = process_updates files in
        let workload = match callback with
        | None -> workload
        | Some callback ->
          if FilenameSet.is_empty files
          then begin
            (* Call the callback immediately if there's nothing to recheck *)
            callback None;
            workload
          end else
            { workload with profiling_callbacks = callback :: workload.profiling_callbacks; }
        in
        if focus
        then { workload with files_to_focus = FilenameSet.union files workload.files_to_focus; }
        else { workload with files_to_recheck = FilenameSet.union files workload.files_to_recheck; }
    )
let get_and_clear_recheck_workload ~process_updates =
  recheck_fetch ~process_updates;
  let recheck_workload = !recheck_acc in
  recheck_acc := empty_recheck_workload;
  recheck_workload
let rec wait_for_updates_for_recheck ~process_updates =
  let%lwt _ = Lwt_stream.is_empty recheck_stream in
  recheck_fetch ~process_updates;
  if recheck_workload_is_empty !recheck_acc
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
