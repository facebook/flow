(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

external realpath: string -> string option = "hh_realpath"

let getenv_user () =
  let user_var = if Sys.win32 then "USERNAME" else "USER" in
  let logname_var = "LOGNAME" in
  try Some (Sys.getenv user_var) with Not_found ->
  try Some (Sys.getenv logname_var) with Not_found -> None

let getenv_home () =
  let home_var = if Sys.win32 then "APPDATA" else "HOME" in
  try Some (Sys.getenv home_var) with Not_found -> None

let getenv_term () =
  let term_var = "TERM" in (* This variable does not exist on windows. *)
  try Some (Sys.getenv term_var) with Not_found -> None

let path_sep = if Sys.win32 then ";" else ":"

let getenv_path () =
  let path_var = "PATH" in (* Same variable on windows *)
  try Some (Sys.getenv path_var) with Not_found -> None

let open_in_no_fail fn =
  try open_in fn
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_in: '%s' (%s)\n" fn e;
    exit 3

let open_in_bin_no_fail fn =
  try open_in_bin fn
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_in: '%s' (%s)\n" fn e;
    exit 3

let close_in_no_fail fn ic =
  try close_in ic with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let open_out_no_fail fn =
  try open_out fn
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_out: '%s' (%s)\n" fn e;
    exit 3

let close_out_no_fail fn oc =
  try close_out oc with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let cat filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in ic;
  content

let cat_no_fail filename =
  let ic = open_in_bin_no_fail filename in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  let content = Buffer.contents buf in
  close_in_no_fail filename ic;
  content

let nl_regexp = Str.regexp "[\r\n]"
let split_lines = Str.split nl_regexp

let exec_read cmd =
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  result

let restart () =
  let cmd = Sys.argv.(0) in
  let argv = Sys.argv in
  Unix.execv cmd argv

let logname_impl () =
  match getenv_user () with
    | Some user -> user
    | None ->
      (* If this function is generally useful, it can be lifted to toplevel
         in this file, but this is the only place we need it for now. *)
      let exec_try_read cmd =
        let ic = Unix.open_process_in cmd in
        let out = try Some (input_line ic) with End_of_file -> None in
        let status = Unix.close_process_in ic in
        match out, status with
          | Some _, Unix.WEXITED 0 -> out
          | _ -> None in
      try Utils.unsafe_opt (exec_try_read "logname") with Invalid_argument _ ->
      try Utils.unsafe_opt (exec_try_read "id -un") with Invalid_argument _ ->
        "[unknown]"

let logname_ref = ref None
let logname () =
  if !logname_ref = None then logname_ref := Some (logname_impl ());
  Utils.unsafe_opt !logname_ref

let with_umask umask f =
  let old_umask = ref 0 in
  Utils.with_context
    ~enter:(fun () -> old_umask := Unix.umask umask)
    ~exit:(fun () -> Unix.umask !old_umask)
    ~do_:f
let with_umask umask f =
  if Sys.win32 then f () else with_umask umask f

let with_timeout timeout ~on_timeout ~do_ =
  let old_handler = ref Sys.Signal_default in
  let old_timeout = ref 0 in
  Utils.with_context
    ~enter:(fun () ->
        old_handler := Sys.signal Sys.sigalrm (Sys.Signal_handle on_timeout);
        old_timeout := Unix.alarm timeout)
    ~exit:(fun () ->
        ignore (Unix.alarm !old_timeout);
        Sys.set_signal Sys.sigalrm !old_handler)
    ~do_

let with_timeout timeout ~on_timeout ~do_ =
  if Sys.win32 then
    do_ () (* TODO *)
  else
    with_timeout timeout ~on_timeout ~do_

let read_stdin_to_string () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

(**
 * Like Python's os.path.expanduser, though probably doesn't cover some cases.
 * Roughly follow's bash's tilde expansion:
 * http://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html
 *
 * ~/foo -> /home/bob/foo if $HOME = "/home/bob"
 * ~joe/foo -> /home/joe/foo if joe's home is /home/joe
 *)
let expanduser path =
  Str.substitute_first
    (Str.regexp "^~\\([^/]*\\)")
    begin fun s ->
      match Str.matched_group 1 s with
        | "" ->
          begin
            match getenv_home () with
              | None -> (Unix.getpwuid (Unix.getuid())).Unix.pw_dir
              | Some home -> home
          end
        | unixname ->
          try (Unix.getpwnam unixname).Unix.pw_dir
          with Not_found -> Str.matched_string s end
    path

(* Turns out it's surprisingly complex to figure out the path to the current
   executable, which we need in order to extract its embedded libraries. If
   argv[0] is a path, then we can use that; sometimes it's just the exe name,
   so we have to search $PATH for it the same way shells do. for example:
   https://www.gnu.org/software/bash/manual/html_node/Command-Search-and-Execution.html

   There are other options which might be more reliable when they exist, like
   using the `_` env var set by bash, or /proc/self/exe on Linux, but they are
   not portable. *)
let executable_path : unit -> string =
  let executable_path_ = ref None in
  let dir_sep = Filename.dir_sep.[0] in
  let search_path path =
    let paths =
      match getenv_path () with
        | None -> failwith "Unable to determine executable path"
        | Some paths ->
          Str.split (Str.regexp_string path_sep) paths in
    let path = List.fold_left paths ~f:begin fun acc p ->
      match acc with
      | Some _ -> acc
      | None -> realpath (expanduser (Filename.concat p path))
    end ~init:None
    in
    match path with
    | Some path -> path
    | None -> failwith "Unable to determine executable path"
  in
  fun () -> match !executable_path_ with
  | Some path -> path
  | None ->
      let path = Sys.executable_name in
      let path =
        if String.contains path dir_sep then
          match realpath path with
          | Some path -> path
          | None -> failwith "Unable to determine executable path"
        else search_path path
      in
      executable_path_ := Some path;
      path

let lines_of_in_channel ic =
  let rec loop accum =
    match try Some(input_line ic) with e -> None with
    | None -> List.rev accum
    | Some(line) -> loop (line::accum)
  in
  loop []

let lines_of_file filename =
  let ic = open_in filename in
  try
    let result = lines_of_in_channel ic in
    let _ = close_in ic in
    result
  with _ ->
    close_in ic;
    []


let read_file file =
  let ic = open_in_bin file  in
  let size = in_channel_length ic in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let write_file ~file s =
  let chan = open_out file in
  (output_string chan s; close_out chan)

let append_file ~file s =
  let chan = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 file in
  (output_string chan s; close_out chan)

(* could be in control section too *)

let filemtime file =
  (Unix.stat file).Unix.st_mtime

external lutimes : string -> unit = "hh_lutimes"

let try_touch ~follow_symlinks file =
  try
    if follow_symlinks then Unix.utimes file 0.0 0.0
    else lutimes file
  with _ ->
    ()

(* Emulate "mkdir -p", i.e., no error if already exists. *)
let mkdir_no_fail dir =
  with_umask 0 begin fun () ->
    (* Don't set sticky bit since the socket opening code wants to remove any
     * old sockets it finds, which may be owned by a different user. *)
    try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let unlink_no_fail fn =
  try Unix.unlink fn with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let splitext filename =
  let root = Filename.chop_extension filename in
  let root_length = String.length root in
  (* -1 because the extension includes the period, e.g. ".foo" *)
  let ext_length = String.length filename - root_length - 1 in
  let ext = String.sub filename (root_length + 1) ext_length in
  root, ext

let is_test_mode () =
  try
    ignore @@ Sys.getenv "HH_TEST_MODE";
    true
  with _ -> false

let symlink =
  (* Dummy implementation of `symlink` on Windows: we create a text
     file containing the targeted-file's path. Symlink are available
     on Windows since Vista, but until Seven (included), one should
     have administratrive rights in order to create symlink. *)
  let win32_symlink source dest = write_file ~file:dest source in
  if Sys.win32 then win32_symlink else Unix.symlink

let setsid =
  (* Not implemented on Windows. Let's just return the pid *)
  if Sys.win32 then Unix.getpid else Unix.setsid

let set_signal = if not Sys.win32 then Sys.set_signal else (fun _ _ -> ())

external get_total_ram : unit -> int = "hh_sysinfo_totalram"
external nproc: unit -> int = "nproc"

let total_ram = get_total_ram ()
let nbr_procs = nproc ()
