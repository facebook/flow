(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a in_channel = Timeout.in_channel
type 'a out_channel = Pervasives.out_channel

type ('in_, 'out) channel_pair = 'in_ in_channel * 'out out_channel

type ('in_, 'out) handle = {
  channels : ('in_, 'out) channel_pair;
  pid : int;
}

let to_channel :
  'a out_channel -> ?flags:Marshal.extern_flags list -> ?flush:bool ->
  'a -> unit =
  fun oc ?(flags = []) ?flush:(should_flush=true) v ->
    Marshal.to_channel oc v flags;
    if should_flush then flush oc

let from_channel : ?timeout:Timeout.t -> 'a in_channel -> 'a = fun ?timeout ic ->
  Timeout.input_value ?timeout ic

let flush : 'a out_channel -> unit = Pervasives.flush

let descr_of_in_channel : 'a in_channel -> Unix.file_descr =
  Timeout.descr_of_in_channel

let descr_of_out_channel : 'a out_channel -> Unix.file_descr =
  Unix.descr_of_out_channel

(* We cannot fork() on Windows, so in order to emulate this in a
 * cross-platform way, we use create_process() and set the HH_SERVER_DAEMON
 * environment variable to indicate which function the child should
 * execute. On Unix, create_process() does fork + exec, so global state is
 * not copied; in particular, if you have set a mutable reference the
 * daemon will not see it. All state must be explicitly passed via
 * environment variables; see set/get_context() below.
 *
 * With some factoring we could make the daemons into separate binaries
 * altogether and dispense with this emulation. *)

module Entry : sig

  (* All the 'untyped' operations---that are required for the
     entry-points hashtable and the parameters stored in env
     variable---are hidden in this sub-module, behind a 'type-safe'
     interface. *)

  type ('param, 'input, 'output) t
  val register:
    string -> ('param -> ('input, 'output) channel_pair -> unit) ->
    ('param, 'input, 'output) t
  val find:
    ('param, 'input, 'output) t ->
    'param ->
    ('input, 'output) channel_pair -> unit
  val set_context:
    ('param, 'input, 'output) t -> 'param ->
    Unix.file_descr * Unix.file_descr ->
    unit
  val get_context:
    unit ->
    (('param, 'input, 'output) t * 'param * ('input, 'output) channel_pair)

end = struct

  type ('param, 'input, 'output) t = string

  (* Store functions as 'Obj.t' *)
  let entry_points : (string, Obj.t) Hashtbl.t = Hashtbl.create 23
  let register name f =
    if Hashtbl.mem entry_points name then
      Printf.ksprintf failwith
        "Daemon.register_entry_point: duplicate entry point %S."
        name;
    Hashtbl.add entry_points name (Obj.repr f);
    name

  let find name =
    try Obj.obj (Hashtbl.find entry_points name)
    with Not_found ->
      Printf.ksprintf failwith
        "Unknown entry point %S" name

  let set_context entry param (ic, oc) =
    let data =
      (Handle.get_handle ic,
       Handle.get_handle oc,
       param) in
    let data_str = String.escaped (Marshal.to_string data []) in
    Unix.putenv "HH_SERVER_DAEMON" entry;
    Unix.putenv "HH_SERVER_DAEMON_PARAM" data_str

  (* How this works on Unix: It may appear like we are passing file descriptors
   * from one process to another here, but in_handle / out_handle are actually
   * file descriptors that are already open in the current process -- they were
   * created by the parent process before it did fork + exec. However, since
   * exec causes the child to "forget" everything, we have to pass the numbers
   * of these file descriptors as arguments.
   *
   * I'm not entirely sure what this does on Windows. *)
  let get_context () =
    let entry = Unix.getenv "HH_SERVER_DAEMON" in
    let (in_handle, out_handle, param) =
      try
        let raw = Sys.getenv "HH_SERVER_DAEMON_PARAM" in
        Marshal.from_string (Scanf.unescaped raw) 0
      with _ -> failwith "Can't find daemon parameters." in
    (entry, param,
     (Timeout.in_channel_of_descr (Handle.wrap_handle in_handle),
      Unix.out_channel_of_descr (Handle.wrap_handle out_handle)))

end

type ('param, 'input, 'output) entry = ('param, 'input, 'output) Entry.t

let exec entry param ic oc =
  let f = Entry.find entry in
  try f param (ic, oc); exit 0
  with e ->
    prerr_endline (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 2

let register_entry_point = Entry.register

let null_path = Path.to_string Path.null_path

let make_pipe () =
  let descr_in, descr_out = Unix.pipe () in
  (* close descriptors on exec so they are not leaked *)
  Unix.set_close_on_exec descr_in;
  Unix.set_close_on_exec descr_out;
  let ic = Timeout.in_channel_of_descr descr_in in
  let oc = Unix.out_channel_of_descr descr_out in
  ic, oc

(* This only works on Unix, and should be avoided as far as possible. Use
 * Daemon.spawn instead. *)
let fork ?log_file (f : ('a, 'b) channel_pair -> unit) :
    ('b, 'a) handle =
  let parent_in, child_out = make_pipe () in
  let child_in, parent_out = make_pipe () in
  match Fork.fork () with
  | -1 -> failwith "Go get yourself a real computer"
  | 0 -> (* child *)
      Timeout.close_in parent_in;
      close_out parent_out;
      Sys_utils.with_umask 0o111 begin fun () ->
        let fd =
          Unix.openfile null_path [Unix.O_RDONLY; Unix.O_CREAT] 0o777 in
        Unix.dup2 fd Unix.stdin;
        Unix.close fd;
        let fn = Option.value_map log_file ~default:null_path ~f:
          begin fun fn ->
            Sys_utils.mkdir_no_fail (Filename.dirname fn);
            fn
          end in
        let fd =
          Unix.openfile fn [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
        Unix.dup2 fd Unix.stdout;
        Unix.dup2 fd Unix.stderr;
        Unix.close fd;
      end;
      f (child_in, child_out);
      exit 0
  | pid -> (* parent *)
      Timeout.close_in child_in;
      close_out child_out;
      { channels = parent_in, parent_out; pid }

let spawn
    (type param) (type input) (type output)
    ?reason ?log_file
    (entry: (param, input, output) entry)
    (param: param) : (output, input) handle =
  let parent_in, child_out = Unix.pipe () in
  let child_in, parent_out = Unix.pipe () in
  (* Close descriptors on exec so they are not leaked. *)
  Unix.set_close_on_exec parent_in;
  Unix.set_close_on_exec parent_out;
  Entry.set_context entry param (child_in, child_out);
  let null_fd =
    Unix.openfile null_path [Unix.O_RDONLY; Unix.O_CREAT] 0o777 in
  let out_path =
    Option.value_map log_file
      ~default:null_path
      ~f:(fun fn ->
          Sys_utils.mkdir_no_fail (Filename.dirname fn);
          fn)  in
  let out_fd =
    Unix.openfile out_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
  let exe = Sys_utils.executable_path () in
  let pid = Unix.create_process exe [|exe|] null_fd out_fd out_fd in
  Option.iter reason ~f:(fun reason -> PidLog.log ~reason pid);
  Unix.close child_in;
  Unix.close child_out;
  Unix.close out_fd;
  Unix.close null_fd;
  { channels = Timeout.in_channel_of_descr parent_in,
               Unix.out_channel_of_descr parent_out;
    pid }

(* for testing code *)
let devnull () =
  let ic = Timeout.open_in "/dev/null" in
  let oc = open_out "/dev/null" in
  {channels = ic, oc; pid = 0}

let check_entry_point () =
  try
    let entry, param, (ic, oc) = Entry.get_context () in
    exec entry param ic oc
  with Not_found -> ()

let close { channels = (ic, oc); _ } =
  Timeout.close_in ic;
  close_out oc

let kill h =
  close h;
  Sys_utils.terminate_process h.pid
