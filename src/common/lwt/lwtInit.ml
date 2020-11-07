(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

(* So there's a bug in Unix.select on Windows. Basically, select is supposed to be
 * "level triggered". That is, as long as you select a ready fd, select should always return it.
 * This is opposed to an "edge triggered" API  which would only return the fd when it becomes
 * available.
 *
 * However, Unix.select doesn't seem to behave this way on Windows. If you call Unix.select with
 * only sockets, then it uses the select system call  and everything is fine. But if you call
 * Unix.select with a mix of fd types (like stdin and a socket, or a pipe and a socket), it
 * falls back to some other code.
 *
 * The long and the short of it is that sometimes a ready fd isn't returned by Unix.select on
 * Windows when you pass in a mix of sockets and non-sockets. The work around is to split the
 * single Unix.select call into multiple Unix.select calls.
 *
 * OCaml bug report: https://caml.inria.fr/mantis/view.php?id=7665
 * lwt issue: https://github.com/ocsigen/lwt/issues/496
 *)
class windows_select =
  object
    inherit Lwt_engine.select_based

    method private select fds_r fds_w timeout =
      (* Figure out which fds are already ready to be read *)
      let ready_r =
        List.fold_left
          (fun ready_r fd_r ->
            match Unix.select [fd_r] [] [] 0.0 with
            | ([], _, _) -> ready_r
            | _ -> fd_r :: ready_r)
          []
          fds_r
      in
      (* Figure out which fds are already ready to be written *)
      let ready_w =
        List.fold_left
          (fun ready_w fd_w ->
            match Unix.select [] [fd_w] [] 0.0 with
            | (_, [], _) -> ready_w
            | _ -> fd_w :: ready_w)
          []
          fds_w
      in
      (* If nothing is ready, then do a multi-fd select with the timeout *)
      if ready_r = [] && ready_w = [] then
        let (fds_r, fds_w, _) = Unix.select fds_r fds_w [] timeout in
        (fds_r, fds_w)
      else
        (ready_r, ready_w)
  end

(*
 * So there's a bug in Unix.select on unix (Linux and OSX). Basically, select is supposed to raise
 * EBADF on bad fds, but it raises EINVAL on fds that are < 0 or >= FD_SETSIZE. Lwt can handle EBADF
 * and will filter out bad fds. However, it doesn't handle EINVAL.
 *
 * So until this is fixed, let's just translate EINVAL to EBADF. This is only dangerous if EINVAL
 * is ever thrown for something other than a bad fd. From the man page, EINVAL is only thrown for
 *
 * 1) invalid timeouts (impossible from the OCaml API)
 * 2) nfds is < 0 or > RLIMIT_NOFILE, which also should be impossible due the c code in select.c
 *
 * So the only case we really need to worry about is fds that are larger than FD_SETSIZE but still
 * valid. From select's manpage:
 *
 * "select() can monitor only file descriptors numbers that are less than FD_SETSIZE;
 *  poll(2) does not have this limitation"
 *
 * So we're kind of screwed if we have large fds we need to monitor anyway.
 *
 * OCaml bug report: https://caml.inria.fr/mantis/view.php?id=7700
 * lwt issue: https://github.com/ocsigen/lwt/issues/529
 *)
class unix_select =
  object
    inherit Lwt_engine.select_based

    method private select fds_r fds_w timeout =
      let (fds_r, fds_w, _) =
        try Unix.select fds_r fds_w [] timeout
        with Unix.Unix_error (Unix.EINVAL, fn, params) ->
          (* Ok, so either one of the fds is an invalid fd, or maybe it's a valid fd but too large
           * for select *)
          begin
            try
              let explode_if_bad fd = Unix.fstat fd |> ignore in
              List.iter explode_if_bad fds_r;
              List.iter explode_if_bad fds_w
            with Unix.Unix_error (_, _, _) -> raise (Unix.Unix_error (Unix.EBADF, fn, params))
          end;

          (* Oh boy. So it looks like all the fds are valid. This likely means that one fd is larger
           * than FD_SETSIZE (which is probably 1024). select() stops working for large fds like this
           *)
          let string_of_fd fd = string_of_int (Obj.magic fd : int) in
          let string_of_fds fds = String.concat ";" (Base.List.map ~f:string_of_fd fds) in
          let params = spf "[%s] [%s] []" (string_of_fds fds_r) (string_of_fds fds_w) in
          raise (Unix.Unix_error (Unix.EINVAL, "select", params))
      in
      (fds_r, fds_w)
  end

let set_engine () =
  (* In theory, we could allow Flow built on machines with libev to use libev instead of select.
   * However, it seems like lwt_config.h on my OSX opam and my CentOS opam both comment out
   * HAVE_LIBEV. And I suppose if we can't rely on libev everywhere then we should rely on it
   * nowhere *)
  if Sys.win32 then
    Lwt_engine.set (new windows_select)
  (* See comment on windows_select *)
  else
    Lwt_engine.set (new unix_select)

exception WrappedException of Exception.t

(* See comment on unix_select *)

let run_lwt f =
  set_engine ();
  try
    Lwt_main.run
      (try%lwt f ()
       with exn ->
         let exn = Exception.wrap exn in
         raise (WrappedException exn))
  with WrappedException exn -> Exception.reraise exn
