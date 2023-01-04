(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let realpath p =
  try Some (Unix.realpath p) with
  | Unix.Unix_error _ -> None

let getenv_home () =
  let home_var =
    if Sys.win32 then
      "APPDATA"
    else
      "HOME"
  in
  Sys.getenv_opt home_var

let getenv_term () =
  let term_var = "TERM" in
  (* This variable does not exist on windows. *)
  Sys.getenv_opt term_var

let path_sep =
  if Sys.win32 then
    ";"
  else
    ":"

let null_path =
  if Sys.win32 then
    "nul"
  else
    "/dev/null"

let temp_dir_name =
  if Sys.win32 then
    Filename.get_temp_dir_name ()
  else
    "/tmp"

let getenv_path () =
  let path_var = "PATH" in
  (* Same variable on windows *)
  Sys.getenv_opt path_var

let cat = Disk.cat

let cat_or_failed file =
  try Some (Disk.cat file) with
  | Sys_error _
  | Failure _ ->
    None

let exec_read cmd =
  let ic = Unix.open_process_in cmd in
  let result = input_line ic in
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  result

let exec_read_lines ?(reverse = false) cmd =
  let ic = Unix.open_process_in cmd in
  let result = ref [] in
  (try
     while true do
       result := input_line ic :: !result
     done
   with
  | End_of_file -> ());
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  if not reverse then
    Base.List.rev !result
  else
    !result

let with_umask umask f =
  if Sys.win32 then
    f ()
  else
    let old_umask = Unix.umask umask in
    Exception.protect ~f ~finally:(fun () ->
        let _ = Unix.umask old_umask in
        ()
    )

let read_all ?(buf_size = 4096) ic =
  let buf = Buffer.create buf_size in
  (try
     while true do
       let data = Bytes.create buf_size in
       let bytes_read = input ic data 0 buf_size in
       if bytes_read = 0 then raise Exit;
       Buffer.add_subbytes buf data 0 bytes_read
     done
   with
  | Exit -> ());
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
    begin
      fun s ->
        match Str.matched_group 1 s with
        | "" -> begin
          match getenv_home () with
          | None -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
          | Some home -> home
        end
        | unixname ->
          (try (Unix.getpwnam unixname).Unix.pw_dir with
          | Not_found -> Str.matched_string s)
    end
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
      | Some paths -> Str.split (Str.regexp_string path_sep) paths
    in
    let path =
      Base.List.fold
        paths
        ~f:
          begin
            fun acc p ->
              match acc with
              | Some _ -> acc
              | None -> realpath (expanduser (Filename.concat p path))
          end
        ~init:None
    in
    match path with
    | Some path -> path
    | None -> failwith "Unable to determine executable path"
  in
  fun () ->
    match !executable_path_ with
    | Some path -> path
    | None ->
      let path = Sys.executable_name in
      let path =
        if String.contains path dir_sep then
          match realpath path with
          | Some path -> path
          | None -> failwith "Unable to determine executable path"
        else
          search_path path
      in
      executable_path_ := Some path;
      path

let lines_of_in_channel ic =
  let rec loop accum =
    match
      try Some (input_line ic) with
      | _ -> None
    with
    | None -> Base.List.rev accum
    | Some line -> loop (line :: accum)
  in
  loop []

let lines_of_file filename =
  let ic = open_in filename in
  try
    let result = lines_of_in_channel ic in
    let _ = close_in ic in
    result
  with
  | _ ->
    close_in ic;
    []

let read_file file =
  let ic = open_in_bin file in
  let size = in_channel_length ic in
  let buf = Bytes.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let write_file ~file s = Disk.write_file ~file ~contents:s

(* Emulate "mkdir -p", i.e., no error if already exists. *)
let mkdir_no_fail dir =
  with_umask 0 (fun () ->
      (* Don't set sticky bit since the socket opening code wants to remove any
       * old sockets it finds, which may be owned by a different user. *)
      try Unix.mkdir dir 0o777 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  )

let symlink =
  (* Dummy implementation of `symlink` on Windows: we create a text
     file containing the targeted-file's path. Symlink are available
     on Windows since Vista, but until Seven (included), one should
     have administratrive rights in order to create symlink. *)
  let win32_symlink source dest = write_file ~file:dest source in
  if Sys.win32 then
    win32_symlink
  else
    (* 4.03 adds an optional argument to Unix.symlink that we want to ignore
     *)
    fun source dest ->
  Unix.symlink source dest

let setsid =
  (* Not implemented on Windows. Let's just return the pid *)
  if Sys.win32 then
    Unix.getpid
  else
    Unix.setsid

let set_signal =
  if not Sys.win32 then
    Sys.set_signal
  else
    fun _ _ ->
  ()

let signal =
  if not Sys.win32 then
    fun a b ->
  ignore (Sys.signal a b)
  else
    fun _ _ ->
  ()

external is_rosetta : unit -> bool = "hh_is_rosetta"

let is_rosetta = is_rosetta ()

external get_total_ram : unit -> int = "hh_sysinfo_totalram"

external uptime : unit -> int = "hh_sysinfo_uptime"

external nproc : unit -> int = "nproc"

let total_ram = get_total_ram ()

let nbr_procs = nproc ()

(** There are a bunch of functions that you expect to return a pid,
  like Unix.getpid() and Unix.create_process(). However, on
  Windows, instead of returning the process ID, they return a
  process handle.

  Process handles act like pointers to a process. You can have
  more than one handle that points to a single process (unlike
  pids, where there is a single pid for a process).

  This isn't a problem normally, since functons like Unix.waitpid()
  will take the process handle on Windows. But if you want to print
  or log the pid, then you need to dereference the handle and get
  the pid. And that's what this function does. *)
external pid_of_handle : int -> int = "pid_of_handle"

(** Returns the actual process identifier on all platforms.

  [Unix.getpid ()] on Windows returns the process handle, not the process identifier.
  The handle is what you need for the other APIs that take a "pid", but the actual
  process identifier is unique across processes and what shows up in Task Manager.
  Use [get_pretty_pid] when showing the PID to the user, logging, etc. *)
let get_pretty_pid () = Unix.getpid () |> pid_of_handle

external handle_of_pid_for_termination : int -> int = "handle_of_pid_for_termination"

let terminate_process pid = Unix.kill pid Sys.sigkill

let lstat path =
  (* WTF, on Windows `lstat` fails if a directory path ends with an
     '/' (or a '\', whatever) *)
  Unix.lstat
  @@
  if Sys.win32 && String.ends_with ~suffix:Filename.dir_sep path then
    String.sub path 0 (String.length path - 1)
  else
    path

(** Converts platform-specific directory separators to / *)
let normalize_filename_dir_sep =
  let dir_sep_char = Filename.dir_sep.[0] in
  let no_op path = path in
  let normalize path =
    if String.contains path dir_sep_char then
      String.map
        (fun c ->
          if Char.equal dir_sep_char c then
            '/'
          else
            c)
        path
    else
      path
  in
  if Char.equal dir_sep_char '/' then
    no_op
  else
    normalize

let name_of_signal = function
  | s when s = Sys.sigabrt -> "SIGABRT (Abnormal termination)"
  | s when s = Sys.sigalrm -> "SIGALRM (Timeout)"
  | s when s = Sys.sigfpe -> "SIGFPE (Arithmetic exception)"
  | s when s = Sys.sighup -> "SIGHUP (Hangup on controlling terminal)"
  | s when s = Sys.sigill -> "SIGILL (Invalid hardware instruction)"
  | s when s = Sys.sigint -> "SIGINT (Interactive interrupt (ctrl-C))"
  | s when s = Sys.sigkill -> "SIGKILL (Termination)"
  | s when s = Sys.sigpipe -> "SIGPIPE (Broken pipe)"
  | s when s = Sys.sigquit -> "SIGQUIT (Interactive termination)"
  | s when s = Sys.sigsegv -> "SIGSEGV (Invalid memory reference)"
  | s when s = Sys.sigterm -> "SIGTERM (Termination)"
  | s when s = Sys.sigusr1 -> "SIGUSR1 (Application-defined signal 1)"
  | s when s = Sys.sigusr2 -> "SIGUSR2 (Application-defined signal 2)"
  | s when s = Sys.sigchld -> "SIGCHLD (Child process terminated)"
  | s when s = Sys.sigcont -> "SIGCONT (Continue)"
  | s when s = Sys.sigstop -> "SIGSTOP (Stop)"
  | s when s = Sys.sigtstp -> "SIGTSTP (Interactive stop)"
  | s when s = Sys.sigttin -> "SIGTTIN (Terminal read from background process)"
  | s when s = Sys.sigttou -> "SIGTTOU (Terminal write from background process)"
  | s when s = Sys.sigvtalrm -> "SIGVTALRM (Timeout in virtual time)"
  | s when s = Sys.sigprof -> "SIGPROF (Profiling interrupt)"
  | s when s = Sys.sigbus -> "SIGBUS (Bus error)"
  | s when s = Sys.sigpoll -> "SIGPOLL (Pollable event)"
  | s when s = Sys.sigsys -> "SIGSYS (Bad argument to routine)"
  | s when s = Sys.sigtrap -> "SIGTRAP (Trace/breakpoint trap)"
  | s when s = Sys.sigurg -> "SIGURG (Urgent condition on socket)"
  | s when s = Sys.sigxcpu -> "SIGXCPU (Timeout in cpu time)"
  | s when s = Sys.sigxfsz -> "SIGXFSZ (File size limit exceeded)"
  | other -> string_of_int other

(* The units for each of these fields is seconds, similar to Unix.times().
 * cpu_info and processor_info are constructed by c code (see processor_info.c) so be very
 * careful modifying these types! *)
type cpu_info = {
  cpu_user: float;
  cpu_nice_user: float;
  cpu_system: float;
  cpu_idle: float;
}

type processor_info = {
  proc_totals: cpu_info;
  proc_per_cpu: cpu_info array;
}

external processor_info : unit -> processor_info = "hh_processor_info"

(* We implement timers using sigalarm which means selects can be interrupted. This is a wrapper
 * around EINTR which continues the select if it gets interrupted by a signal *)
let rec select_non_intr read write exn timeout =
  let start_time = Unix.gettimeofday () in
  try Unix.select read write exn timeout with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
    (* Negative timeouts mean no timeout *)
    let timeout =
      if timeout < 0.0 then
        timeout
      else
        max 0.0 (timeout -. (Unix.gettimeofday () -. start_time))
    in
    select_non_intr read write exn timeout

(* Flow uses lwt, which installs a sigchld handler. So the old pattern of fork & waitpid will hit
 * an EINTR when the forked process dies and the parent gets a sigchld signal. Note: this is only a
 * problem if you're not using the WNOHANG flag, since EINTR isn't thrown for WNOHANG *)
let rec waitpid_non_intr flags pid =
  try Unix.waitpid flags pid with
  | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr flags pid

(* Exposing this for a unit test *)
let find_oom_in_dmesg_output pid name lines =
  (* oomd: "Memory cgroup out of memory: Killed process 4083583 (flow)" (https://facebookmicrosites.github.io/oomd/)
     oomkiller: "Out of memory: Kill process 4083583 (flow)" *)
  let re =
    Str.regexp (Printf.sprintf "[Oo]ut of memory: Kill\\(ed\\)? process \\([0-9]+\\) (%s)" name)
  in
  let pid = string_of_int pid in
  Base.List.exists lines ~f:(fun line ->
      try
        ignore @@ Str.search_forward re line 0;
        let pid_s = Str.matched_group 2 line in
        pid_s = pid
      with
      | Not_found -> false
  )

let check_dmesg_for_oom pid name =
  let dmesg = exec_read_lines ~reverse:true "dmesg" in
  find_oom_in_dmesg_output pid name dmesg

external start_gc_profiling : unit -> unit = "hh_start_gc_profiling" [@@noalloc]

external get_gc_time : unit -> float * float = "hh_get_gc_time"
