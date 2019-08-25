(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)
open Hh_core

module Make (EnvType : sig
  type t
end) =
struct
  type t = EnvType.t

  type job = {
    priority: int;
    run: t -> t;
  }

  type wait_handle =
    (* Job that should be run if provided function tells us that there is
     * something to do *)
    | Fun of (t -> bool) * job
    (* Job that should be run when file descriptor is ready *)
    | Channel of Unix.file_descr * job

  type env = {
    waiting_jobs: wait_handle list;
    ready_jobs: job list;
  }

  let empty () = { waiting_jobs = []; ready_jobs = [] }

  let env = ref (empty ())

  let reset () = env := empty ()

  let rec wait_for_fun ?(once = false) ~priority is_ready f =
    let f' =
      if once then
        f
      else
        fun job_env ->
      wait_for_fun ~priority is_ready f;
      let job_env = f job_env in
      job_env
    in
    let wait_handle = Fun (is_ready, { priority; run = f' }) in
    env := { !env with waiting_jobs = wait_handle :: !env.waiting_jobs }

  let rec wait_for_channel ~priority fd f =
    let f' env =
      wait_for_channel ~priority fd f;
      f env
    in
    let wait_handle = Channel (fd, { priority; run = f' }) in
    env := { !env with waiting_jobs = wait_handle :: !env.waiting_jobs }

  let stop_waiting_for_channel fd =
    let waiting_jobs =
      List.filter !env.waiting_jobs (function
          | Channel (x, _) -> x <> fd
          | _ -> true)
    in
    env := { !env with waiting_jobs }

  let wait_for_ready_jobs job_env =
    let (funs, channels) =
      List.partition_map !env.waiting_jobs ~f:(function
          | Fun (x, y) -> `Fst (x, y)
          | Channel (fd, f) -> `Snd (fd, f))
    in
    let (ready_funs, waiting_funs) =
      List.partition_map funs ~f:(fun (is_ready, job) ->
          if is_ready job_env then
            `Fst job
          else
            `Snd (Fun (is_ready, job)))
    in
    let wait_time =
      if ready_funs = [] && !env.ready_jobs = [] then
        1.0
      else
        0.0
    in
    let fds = List.map channels ~f:fst in
    let (readable, _, _) = Unix.select fds [] [] wait_time in
    let (ready_channels, waiting_channels) =
      List.partition_map channels ~f:(fun (fd, job) ->
          if List.exists readable ~f:(fun x -> x = fd) then
            `Fst job
          else
            `Snd (Channel (fd, job)))
    in
    let ready_jobs = ready_funs @ ready_channels @ !env.ready_jobs in
    let ready_jobs =
      List.sort ready_jobs ~cmp:(fun x y -> x.priority - y.priority)
    in
    env := { ready_jobs; waiting_jobs = waiting_funs @ waiting_channels }

  let rec wait_and_run_ready job_env =
    wait_for_ready_jobs job_env;
    let job =
      match !env.ready_jobs with
      | h :: t ->
        env := { !env with ready_jobs = t };
        Some h
      | [] -> None
    in
    match job with
    | Some job ->
      let job_env = job.run job_env in
      wait_and_run_ready job_env
    | None -> job_env
end
