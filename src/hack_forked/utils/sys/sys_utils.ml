(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core

external realpath : string -> string option = "hh_realpath"

external is_nfs : string -> bool = "hh_is_nfs"

external is_apple_os : unit -> bool = "hh_sysinfo_is_apple_os"

(** Option type intead of exception throwing. *)
let get_env name = (try Some (Sys.getenv name) with Not_found -> None)

let getenv_user () =
  let user_var =
    if Sys.win32 then
      "USERNAME"
    else
      "USER"
  in
  let logname_var = "LOGNAME" in
  let user = get_env user_var in
  let logname = get_env logname_var in
  Base.Option.first_some user logname

let getenv_home () =
  let home_var =
    if Sys.win32 then
      "APPDATA"
    else
      "HOME"
  in
  get_env home_var

let getenv_term () =
  let term_var = "TERM" in
  (* This variable does not exist on windows. *)
  get_env term_var

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
  get_env path_var

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
    Printf.fprintf stderr "Could not open_in_bin: '%s' (%s)\n" fn e;
    exit 3

let close_in_no_fail fn ic =
  try close_in ic
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let open_out_no_fail fn =
  try open_out fn
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_out: '%s' (%s)\n" fn e;
    exit 3

let open_out_bin_no_fail fn =
  try open_out_bin fn
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not open_out_bin: '%s' (%s)\n" fn e;
    exit 3

let close_out_no_fail fn oc =
  try close_out oc
  with e ->
    let e = Printexc.to_string e in
    Printf.fprintf stderr "Could not close: '%s' (%s)\n" fn e;
    exit 3

let cat = Disk.cat

let cat_or_failed file =
  try Some (Disk.cat file) with
  | Sys_error _
  | Failure _ ->
    None

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

(** Returns true if substring occurs somewhere inside str. *)
let string_contains str substring =
  (* regexp_string matches only this string and nothing else. *)
  let re = Str.regexp_string substring in
  (try Str.search_forward re str 0 >= 0 with Not_found -> false)

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
   with End_of_file -> ());
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  if not reverse then
    List.rev !result
  else
    !result

(**
 * Collects paths that satisfy a predicate, recursively traversing directories.
 *)
let rec collect_paths path_predicate path =
  if Sys.is_directory path then
    path
    |> Sys.readdir
    |> Array.to_list
    |> List.map ~f:(Filename.concat path)
    |> List.concat_map ~f:(collect_paths path_predicate)
  else
    Utils.singleton_if (path_predicate path) path

(**
 * Sometimes the user wants to pass a list of paths on the command-line.
 * However, we have enough files in the codebase that sometimes that list
 * exceeds the maximum number of arguments that can be passed on the
 * command-line. To work around this, we can use the convention that some Unix
 * tools use: a `@` before a path name represents a file that should be read
 * to get the necessary information (in this case, containing a list of files,
 * one per line).
 *)
let parse_path_list (paths : string list) : string list =
  List.concat_map paths ~f:(fun path ->
      if String_utils.string_starts_with path "@" then
        let path = String_utils.lstrip path "@" in
        cat path |> split_lines
      else
        [path])
  |> List.map ~f:(fun path ->
         match realpath path with
         | Some path -> path
         | None -> failwith (Printf.sprintf "Invalid path: %s" path))

let rm_dir_tree ?(skip_mocking = false) =
  if skip_mocking then
    RealDisk.rm_dir_tree
  else
    Disk.rm_dir_tree

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
      let out = (try Some (input_line ic) with End_of_file -> None) in
      let status = Unix.close_process_in ic in
      match (out, status) with
      | (Some _, Unix.WEXITED 0) -> out
      | _ -> None
    in
    (try Utils.unsafe_opt (exec_try_read "logname")
     with Invalid_argument _ ->
       (try Utils.unsafe_opt (exec_try_read "id -un") with Invalid_argument _ -> "[unknown]"))

let logname_ref = ref None

let logname () =
  if !logname_ref = None then logname_ref := Some (logname_impl ());
  Utils.unsafe_opt !logname_ref

let with_umask umask f =
  let old_umask = ref 0 in
  Utils.with_context
    ~enter:(fun () -> old_umask := Unix.umask umask)
    ~exit:(fun () ->
      let _ = Unix.umask !old_umask in
      ())
    ~do_:f

let with_umask umask f =
  if Sys.win32 then
    f ()
  else
    with_umask umask f

let read_stdin_to_string () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file -> Buffer.contents buf

let read_all ?(buf_size = 4096) ic =
  let buf = Buffer.create buf_size in
  (try
     while true do
       let data = Bytes.create buf_size in
       let bytes_read = input ic data 0 buf_size in
       if bytes_read = 0 then raise Exit;
       Buffer.add_subbytes buf data 0 bytes_read
     done
   with Exit -> ());
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
      | "" ->
        begin
          match getenv_home () with
          | None -> (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
          | Some home -> home
        end
      | unixname ->
        (try (Unix.getpwnam unixname).Unix.pw_dir with Not_found -> Str.matched_string s)
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
      List.fold_left
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
    match (try Some (input_line ic) with _ -> None) with
    | None -> List.rev accum
    | Some line -> loop (line :: accum)
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
  let ic = open_in_bin file in
  let size = in_channel_length ic in
  let buf = Bytes.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let write_file ~file s = Disk.write_file ~file ~contents:s

let append_file ~file s =
  let chan = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 file in
  output_string chan s;
  close_out chan

let write_strings_to_file ~file (ss : string list) =
  let chan = open_out_gen [Open_wronly; Open_creat] 0o666 file in
  List.iter ~f:(output_string chan) ss;
  close_out chan

(* could be in control section too *)

let filemtime file = (Unix.stat file).Unix.st_mtime

external lutimes : string -> unit = "hh_lutimes"

let try_touch ~follow_symlinks file =
  try
    if follow_symlinks then
      Unix.utimes file 0.0 0.0
    else
      lutimes file
  with _ -> ()

let mkdir_p ?(skip_mocking = false) =
  if skip_mocking then
    RealDisk.mkdir_p
  else
    Disk.mkdir_p

(* Emulate "mkdir -p", i.e., no error if already exists. *)
let mkdir_no_fail dir =
  with_umask 0 (fun () ->
      (* Don't set sticky bit since the socket opening code wants to remove any
       * old sockets it finds, which may be owned by a different user. *)
      try Unix.mkdir dir 0o777 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let unlink_no_fail fn = (try Unix.unlink fn with Unix.Unix_error (Unix.ENOENT, _, _) -> ())

let readlink_no_fail fn =
  if Sys.win32 && Sys.file_exists fn then
    cat fn
  else
    try Unix.readlink fn with _ -> fn

let splitext filename =
  let root = Filename.chop_extension filename in
  let root_length = String.length root in
  (* -1 because the extension includes the period, e.g. ".foo" *)
  let ext_length = String.length filename - root_length - 1 in
  let ext = String.sub filename (root_length + 1) ext_length in
  (root, ext)

let is_test_mode () =
  try
    ignore @@ Sys.getenv "HH_TEST_MODE";
    true
  with _ -> false

let sleep ~seconds = ignore @@ Unix.select [] [] [] seconds

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

(* Creates a symlink at <dir>/<linkname.ext> to
 * <dir>/<pluralized ext>/<linkname>-<timestamp>.<ext> *)
let make_link_of_timestamped linkname =
  Unix.(
    let dir = Filename.dirname linkname in
    mkdir_no_fail dir;
    let base = Filename.basename linkname in
    let (base, ext) = splitext base in
    let dir = Filename.concat dir (Printf.sprintf "%ss" ext) in
    mkdir_no_fail dir;
    let tm = localtime (time ()) in
    let year = tm.tm_year + 1900 in
    let time_str =
      Printf.sprintf
        "%d-%02d-%02d-%02d-%02d-%02d"
        year
        (tm.tm_mon + 1)
        tm.tm_mday
        tm.tm_hour
        tm.tm_min
        tm.tm_sec
    in
    let filename = Filename.concat dir (Printf.sprintf "%s-%s.%s" base time_str ext) in
    unlink_no_fail linkname;
    symlink filename linkname;
    filename)

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

external get_total_ram : unit -> int = "hh_sysinfo_totalram"

external uptime : unit -> int = "hh_sysinfo_uptime"

external nproc : unit -> int = "nproc"

let total_ram = get_total_ram ()

let nbr_procs = nproc ()

external set_priorities : cpu_priority:int -> io_priority:int -> unit = "hh_set_priorities"

external pid_of_handle : int -> int = "pid_of_handle"

external handle_of_pid_for_termination : int -> int = "handle_of_pid_for_termination"

let terminate_process pid = Unix.kill pid Sys.sigkill

let lstat path =
  (* WTF, on Windows `lstat` fails if a directory path ends with an
     '/' (or a '\', whatever) *)
  Unix.lstat
  @@
  if Sys.win32 && String_utils.string_ends_with path Filename.dir_sep then
    String.sub path 0 (String.length path - 1)
  else
    path

let normalize_filename_dir_sep =
  let dir_sep_char = Filename.dir_sep.[0] in
  String.map (fun c ->
      if c = dir_sep_char then
        '/'
      else
        c)

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
  try Unix.select read write exn timeout
  with Unix.Unix_error (Unix.EINTR, _, _) ->
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
  try Unix.waitpid flags pid with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr flags pid

(* Exposing this for a unit test *)
let find_oom_in_dmesg_output pid name lines =
  let re = Str.regexp (Printf.sprintf "Out of memory: Kill process \\([0-9]+\\) (%s)" name) in
  List.exists lines (fun line ->
      try
        ignore @@ Str.search_forward re line 0;
        let pid_s = Str.matched_group 1 line in
        int_of_string pid_s = pid
      with Not_found -> false)

let check_dmesg_for_oom pid name =
  let dmesg = exec_read_lines ~reverse:true "dmesg" in
  find_oom_in_dmesg_output pid name dmesg

(* Be careful modifying the rusage type! Like other types that interact with C, the order matters!
 * If you change things here you must update hh_getrusage too! *)
type rusage = {
  ru_maxrss: int;
  (* maximum resident set size *)
  ru_ixrss: int;
  (* integral shared memory size *)
  ru_idrss: int;
  (* integral unshared data size *)
  ru_isrss: int;
  (* integral unshared stack size *)
  ru_minflt: int;
  (* page reclaims (soft page faults) *)
  ru_majflt: int;
  (* page faults (hard page faults) *)
  ru_nswap: int;
  (* swaps *)
  ru_inblock: int;
  (* block input operations *)
  ru_oublock: int;
  (* block output operations *)
  ru_msgsnd: int;
  (* IPC messages sent *)
  ru_msgrcv: int;
  (* IPC messages received *)
  ru_nsignals: int;
  (* signals received *)
  ru_nvcsw: int;
  (* voluntary context switches *)
  ru_nivcsw: int; (* involuntary context switches *)
}

external getrusage : unit -> rusage = "hh_getrusage"

external start_gc_profiling : unit -> unit = "hh_start_gc_profiling" [@@noalloc]

external get_gc_time : unit -> float * float = "hh_get_gc_time"
