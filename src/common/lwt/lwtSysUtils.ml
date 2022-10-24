(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Basically a `waitpid [ WUNTRACED ] pid` (WUNTRACED means also return on stopped processes) *)
let blocking_waitpid =
  let reasonable_impl pid = Lwt_unix.waitpid [Unix.WUNTRACED] pid in
  (* Lwt_unix.waitpid without WNOHANG doesn't work on Windows. As a workaround, we can call the
     * WNOHANG version every .5 seconds. https://github.com/ocsigen/lwt/issues/494 *)
  let rec damn_it_windows_impl pid_to_wait_for =
    let%lwt (pid_ret, status) = Lwt_unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] pid_to_wait_for in
    if pid_ret = 0 then
      (* Still hasn't exited. Let's wait .5s and try again *)
      let%lwt () = Lwt_unix.sleep 0.5 in
      damn_it_windows_impl pid_to_wait_for
    else
      (* Ok, process has exited or died or something. *)
      Lwt.return (pid_ret, status)
  in
  if Sys.win32 then
    damn_it_windows_impl
  else
    reasonable_impl

type command_result = {
  stdout: string;
  stderr: string;
  status: Unix.process_status;
}

let command_result_of_process process =
  (* Wait for it to finish *)
  let%lwt status = process#status
  and stdout = Lwt_io.read process#stdout
  and stderr = Lwt_io.read process#stderr in
  Lwt.return { stdout; stderr; status }

let prepare_args cmd args =
  (* [Lwt_process.spawn] calls Windows' CreateProcess directly, and [Unix.execvp] otherwise.
     [Unix.execvp] searches for [cmd] on the path, but Lwt doesn't (as of Lwt 5.4.2).

     By passing "", [Lwt_process.spawn] leaves the [lpApplicationName] argument to
     [CreateProcess] blank, causing [CreateProcess] to search for the first whitespace-
     delimited token ([cmd]) on the path like we want.

     This also works on Unix because Lwt_process.unix_spawn uses the first array element
     instead, when we pass "". *)
  ("", Array.of_list (cmd :: args))

(** [Lwt_process.spawn] incorrectly serializes the [env] argument on Windows
  (https://github.com/ocsigen/lwt/issues/966). To work around this, ON WINDOWS,
  we set the desired envs in the current process, let the spawned child inherit
  our env, and then undo the changes. On other platforms, we explicitly pass
  [env].

  This function returns [None] on Windows and [Some env], where [env] is the
  child's complete env, otherwise. It also returns a "restore" function that
  will undo the env changes in the current process, if any. This only does
  anything on Windows.

  We only support extending the env rather than passing a clean new env,
  so that we don't have to unset all of the existing envs on Windows and then
  restore them. As of this writing, this is all we need anyway. *)
let prepare_env = function
  | None -> (None, Fun.id)
  | Some (`Extend envs) ->
    if Sys.win32 then
      let prevs =
        List.map
          (fun (k, v) ->
            let prev = Sys.getenv_opt k |> Option.value ~default:"" in
            Unix.putenv k v;
            (k, prev))
          envs
      in
      let restore () = List.iter (fun (k, v) -> Unix.putenv k v) prevs in
      (None, restore)
    else
      (* why do we use [unsafe_environment] and pass a potentially large
         env around instead of the set/restore pattern? only Windows'
         [Unix.putenv] lets you unset a variable by passing an empty string;
         on Linux you would need to call unsetenv() which is not available
         in OCaml. *)
      let envs = List.map (fun (k, v) -> k ^ "=" ^ v) envs |> Array.of_list in
      (Some (Array.append (Unix.unsafe_environment ()) envs), Fun.id)

(** At least as of Lwt 5.5.0, [Lwt_process.with_process_full] tries to close
  the process even when [f] fails, and can raise an EBADF that swallows
  whatever the original exception was. https://github.com/ocsigen/lwt/issues/956

  Instead, we will swallow exceptions from [close] and use our [Exception] to
  reraise the original exception. We also use ppx_lwt instead of [Lwt.finalize]
  to improve backtraces. *)
let with_process_full ?timeout ?env ?cwd cmd f =
  let (env, restore_env) = prepare_env env in
  let process =
    try
      let process = Lwt_process.open_process_full ?timeout ?env ?cwd cmd in
      restore_env ();
      process
    with
    | e ->
      let exn = Exception.wrap e in
      restore_env ();
      Exception.reraise exn
  in
  let ignore_close process =
    try%lwt
      let%lwt _ = process#close in
      Lwt.return_unit
    with
    | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
  in
  let%lwt result =
    try%lwt f process with
    | e ->
      let exn = Exception.wrap e in
      let%lwt () = ignore_close process in
      Exception.reraise exn
  in
  let%lwt () = ignore_close process in
  Lwt.return result

let exec ?env ?cwd cmd args =
  with_process_full ?env ?cwd (prepare_args cmd args) command_result_of_process

let exec_with_timeout ?env ~timeout cmd args =
  with_process_full ?env (prepare_args cmd args) (fun process ->
      let timeout_msg =
        Printf.sprintf "Timed out while running `%s` after %.3f seconds" cmd timeout
      in
      let on_timeout () =
        process#terminate;
        let%lwt _ = process#close in
        Lwt.return_unit
      in
      LwtTimeout.with_timeout ~timeout_msg ~on_timeout timeout (fun () ->
          let%lwt command_result = command_result_of_process process in
          Lwt.return (Ok command_result)
      )
  )
