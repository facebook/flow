(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Tools for shelling out to Mercurial. *)

open Core

exception Malformed_result

type hg_rev = string
type svn_rev = string

(** Uttilities to deal with subprocesses. *)
module Process = struct

  let chunk_size = 65536

  (** Reuse the buffer for reading. Just an allocation optimization. *)
  let buffer = String.create chunk_size

  exception Process_exited_with_error of (Unix.process_status * string)
  exception Select_timed_out

  (** Recursively read from fds. If EOF is reached, remove that
   * fd from fds; terminatin when fds is empty.
   *
   * fd and err_fd helps us track which accumulator to put the read
   * data from. *)
  let rec read_and_close_pid fd err_fd fds pid acc acc_err =
    if fds = [] then
      let () = Unix.close fd in
      let () = Unix.close err_fd in
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 ->
        let result = String.concat "" (List.rev acc) in
        result
      | _, status ->
        let err = String.concat "" (List.rev acc_err) in
        raise (Process_exited_with_error (status, err))
    else
      let ready_fds, _, _ = Unix.select fds [] [] 9999999.9 in
      if ready_fds = [] then
         raise Select_timed_out
      else
      let fds, acc, acc_err = List.fold_left ready_fds ~init:(fds, acc, acc_err)
      ~f:begin fun (fds, acc, acc_err) fd ->
        let bytes_read = Unix.read fd buffer 0 chunk_size in
        if bytes_read = 0 then
          (((List.filter fds ~f:(fun x -> x <> fd))), acc, acc_err)
        else
          let chunk = String.sub buffer 0 bytes_read in
          if fd = err_fd then
            (fds, acc, (chunk :: acc_err))
          else
            (fds, (chunk :: acc), acc_err)
      end
      in
      read_and_close_pid fd err_fd fds pid acc acc_err

   let read_and_close_pid fd err_fd pid =
     try read_and_close_pid fd err_fd [fd; err_fd] pid [] [] with
     | Process_exited_with_error _ as e ->
       let user = Option.value (Sys_utils.getenv_user ()) ~default:"" in
       let home = Option.value (Sys_utils.getenv_home ()) ~default:"" in
       let () = Printf.eprintf
         "Process failed. See also env USER=%s. HOME=%s\n" user home in
       raise e
end


(** We shell out hg commands, so its computation is out-of-process, and
 * return a Future for the result. *)
module Future = struct
  type pid = int
  type 'a promise =
    | Complete : 'a -> 'a promise
    | Incomplete of Unix.file_descr * Unix.file_descr * pid * (string -> 'a)
  type 'a t = 'a promise ref

  let make out_fd err_fd pid transformer =
    ref (Incomplete (out_fd, err_fd, pid, transformer))

  let get : 'a t -> 'a = fun promise -> match !promise with
    | Complete v -> v
    | Incomplete (fd, err_fd, pid, transformer) ->
      let result = Process.read_and_close_pid fd err_fd pid in
      let result = transformer result in
      promise := Complete result;
      result
end

let devnull = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o660

let exec cmd args transformer =
  let args = cmd :: args in
  let in_fd, out_fd = Unix.pipe () in
  let in_err, out_err = Unix.pipe () in
  let pid = Unix.create_process
    "hg" (Array.of_list ("hg" :: args)) devnull out_fd out_err in
  Unix.close out_fd;
  Unix.close out_err;
  Future.make in_fd in_err pid transformer

(** Returns the closest SVN ancestor to the hg revision.
 *
 * hg log -r 'reverse(::<hg_rev>)' -T '{svnrev}\n' -l 1 <repo> *)
let get_closest_svn_ancestor hg_rev repo =
  exec "log" [
    {|-r|};
    {|reverse(::|} ^ hg_rev ^ {|)|};
    {|-T|};
    {|{svnrev}\n|};
    {|-l|};
    {|1|};
    {|--cwd|};
    repo;
  ]
  String.trim

(** Get the hg revision hash of the current working copy in the repo dir.
 *
 * hg id -i --cwd <repo> *)
let current_working_copy_hg_rev repo =
  exec "id" ["-i"; "--cwd"; repo; ] @@ fun result ->
    let result = String.trim result in
    if String.length result < 1 then
      raise Malformed_result
    else
      if result.[(String.length result) - 1] = '+' then
        (String.sub result 0 ((String.length result) - 1)), true
      else
        result, false

(** Returns the files changed between the hg_rev and the ancestor
 * SVN revision.
 *
 * hg status --rev r<svn_rev> --rev <hg_rev> --cwd <repo> *)
let files_changed_since_svn_rev hg_rev svn_rev repo =
  exec "status" [
    {|--rev|};
    ("r" ^ svn_rev);
    {|--rev|};
    hg_rev;
    {|--cwd|};
    repo;
  ]
  String.trim
