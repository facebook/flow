open Core_kernel

let select
    (read_fds : Unix.file_descr list)
    (write_fds : Unix.file_descr list)
    (exn_fds : Unix.file_descr list)
    (timeout : float) :
    (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list) Lwt.t =
  let make_task
      ~(fds : Unix.file_descr list)
      ~(condition : Lwt_unix.file_descr -> bool)
      ~(wait_f : Lwt_unix.file_descr -> unit Lwt.t) :
      (Unix.file_descr list, Unix.file_descr list) result Lwt.t =
    try%lwt
      let fds = List.map fds ~f:Lwt_unix.of_unix_file_descr in
      let%lwt () = Lwt.pick (List.map fds ~f:wait_f) in
      let actionable_fds =
        fds |> List.filter ~f:condition |> List.map ~f:Lwt_unix.unix_file_descr
      in
      Lwt.return (Ok actionable_fds)
    with _ ->
      (* Although we gather a list of exceptional file descriptors here, it
      happens that no call site of `Unix.select` in the codebase has checked
      this list, so we could in theory just return any list (or not return any
      exceptional file descriptors at all). *)
      let exceptional_fds =
        List.filter exn_fds ~f:(fun fd -> List.mem ~equal:( = ) fds fd)
      in
      Lwt.return (Error exceptional_fds)
  in
  let read_task =
    let%lwt readable_fds =
      make_task
        ~fds:read_fds
        ~condition:Lwt_unix.readable
        ~wait_f:Lwt_unix.wait_read
    in
    match readable_fds with
    | Ok fds -> Lwt.return (fds, [], [])
    | Error fds -> Lwt.return ([], [], fds)
  in
  let write_task =
    let%lwt writeable_fds =
      make_task
        ~fds:write_fds
        ~condition:Lwt_unix.writable
        ~wait_f:Lwt_unix.wait_write
    in
    match writeable_fds with
    | Ok fds -> Lwt.return ([], fds, [])
    | Error fds -> Lwt.return ([], [], fds)
  in
  let tasks = [read_task; write_task] in
  let tasks =
    if timeout > 0.0 then
      let timeout_task =
        let%lwt () = Lwt_unix.sleep timeout in
        Lwt.return ([], [], [])
      in
      timeout_task :: tasks
    else
      failwith "Timeout <= 0 not implemented"
  in
  Lwt.pick tasks

module Process_success = struct
  type t = {
    command_line: string;
    stdout: string;
    stderr: string;
  }
end

module Process_failure = struct
  type t = {
    command_line: string;
    process_status: Unix.process_status;
    stdout: string;
    stderr: string;
    exn: exn option;
  }

  let to_string (process_failure : t) : string =
    let exn_message =
      match process_failure.exn with
      | Some exn -> Exn.to_string exn
      | None -> "<none>"
    in
    let exit_code =
      Unix.(
        match process_failure.process_status with
        | WEXITED exit_code -> "WEXITED " ^ string_of_int exit_code
        | WSIGNALED exit_code -> "WSIGNALED " ^ string_of_int exit_code
        | WSTOPPED exit_code -> "WSTOPPED " ^ string_of_int exit_code)
    in
    let stderr =
      match process_failure.stderr with
      | "" -> "<none>"
      | stderr -> stderr
    in
    Printf.sprintf
      ( "Process '%s' failed with\n"
      ^^ "Exception: %s\n"
      ^^ "Exit code: %s\n"
      ^^ "Stderr: %s" )
      process_failure.command_line
      exn_message
      exit_code
      stderr
end

let exec_checked
    ?(input : string option)
    ?(env : string array option)
    (program : string)
    (args : string array) : (Process_success.t, Process_failure.t) Lwt_result.t
    =
  let command_line =
    let args =
      args |> Array.map ~f:(fun x -> " " ^ x) |> String.concat_array ~sep:""
    in
    program ^ args
  in
  let process =
    let command = (program, Array.append [| program |] args) in
    Lwt_process.open_process_full command ?env
  in
  (let%lwt (exn, stdout, stderr) =
     let exn = ref None in
     let stdout = ref "" in
     let stderr = ref "" in
     let%lwt () =
       try%lwt
         let%lwt () =
           match input with
           | Some input ->
             let%lwt () = Lwt_io.write process#stdin input in
             let%lwt () = Lwt_io.close process#stdin in
             Lwt.return_unit
           | None -> Lwt.return_unit
         and () =
           let%lwt result = Lwt_io.read process#stdout in
           stdout := result;
           Lwt.return_unit
         and () =
           let%lwt result = Lwt_io.read process#stderr in
           stderr := result;
           Lwt.return_unit
         in
         Lwt.return_unit
       with e ->
         exn := Some e;
         Lwt.return_unit
     in
     Lwt.return (!exn, !stdout, !stderr)
   in
   let%lwt state = process#close in
   match state with
   | Unix.WEXITED 0 ->
     Lwt.return_ok { Process_success.command_line; stdout; stderr }
   | process_status ->
     Lwt.return_error
       { Process_failure.command_line; process_status; stdout; stderr; exn })
    [%finally
      let%lwt (_ : Unix.process_status) = process#close in
      Lwt.return_unit]

let try_finally ~(f : unit -> 'a Lwt.t) ~(finally : unit -> unit Lwt.t) :
    'a Lwt.t =
  let%lwt res =
    try%lwt
      let%lwt result = f () in
      Lwt.return result
    with e ->
      let%lwt () = finally () in
      raise e
  in
  let%lwt () = finally () in
  Lwt.return res

let read_all (path : string) : (string, string) Lwt_result.t =
  try%lwt
    let%lwt contents =
      Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
          let%lwt contents = Lwt_io.read ic in
          Lwt.return contents)
    in
    Lwt.return (Ok contents)
  with _ ->
    Lwt.return
      (Error
         (Printf.sprintf
            "Could not read the contents of the file at path %s"
            path))

module Promise = struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return

  let map e f = Lwt.map f e

  let bind = Lwt.bind
end
