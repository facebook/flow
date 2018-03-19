(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module makes Lwt's logger easily available for use. Lwt's logger is nice because it
 * can easily log exceptions, format messages, and interact with Lwt.
 *
 * Initially I used Lwt_io for this module's implementation. It was nice, because Lwt_io.atomic
 * guaranteed that I wouldn't have multiple threads interleaving their logs. However, I found that
 * Lwt_io.flush_all which is called by at_exit could take awhile. Furthermore, I was able to
 * trigger deadlocks by logging & flushing around exit time.
 *
 * So now the implementation uses fds exclusively.
 *
 * 1. Multiple threads write to the msg_stream with their logs
 * 2. A single thread (WriteLoop) reads the messages and writes them to the various fds directly
 **)

type 'a logger_fn =
  ?exn : exn ->
  ?section : Lwt_log_core.section ->
  ?location : (string * int * int) ->
  ?logger:Lwt_log_core.logger ->
  ('a, unit, string, unit) format4 ->
  'a

let msg_stream, push_to_msg_stream = Lwt_stream.create ()

module WriteLoop = LwtLoop.Make (struct
  type acc = Lwt_unix.file_descr list

  (* Given a list of messages and a fd, write them serially to the fd *)
  let write_msgs msgs fd =
    Lwt_list.iter_s
      (fun msg ->
        let%lwt _ = Lwt_unix.write_string fd msg 0 (String.length msg) in
        Lwt.return_unit)
      msgs

  (* Get a list of messages, write the list in parallel to each fd *)
  let main fds =
    let%lwt msgs = Lwt_stream.next msg_stream in
    let%lwt () = Lwt_list.iter_p (write_msgs msgs) fds in
    Lwt.return fds

  (* If we failed to write to an fd throw an exception and exit. I'm not 100% sure this is the
   * best behavior - should logging errors cause the monitor (and server) to crash? *)
  let catch _ exn =
    Printf.eprintf
      "Logger.WriteLoop exception:\n%s\n%s"
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());
    raise exn
end)

let initialized = ref false

(* Creates a default logger and sets the minimum logger level. The logger will log every message
 * that passes the minimum level to stderr. If log_fd is provided, each message will be logged
 * to it as well *)
let init_logger ?log_fd min_level =
  if !initialized then failwith "Cannot initialized FlowServerMonitorLogger more than once";
  initialized := true;

  let template = "$(date).$(milliseconds) [$(level)] $(message)" in

  let log_fd = Option.map log_fd ~f:Lwt_unix.of_unix_file_descr in

  let fds = Lwt_unix.stderr :: (Option.value_map log_fd ~default:[] ~f:(fun fd -> [fd])) in
  Lwt.async (fun () -> WriteLoop.run fds);

  (* Format the messages and write the to the log and stderr *)
  let output section level messages =
    let buffer = Buffer.create 42 in
    let formatted_messages = List.map (fun message ->
      Buffer.clear buffer;
      Lwt_log.render ~buffer ~template ~section ~level ~message;
      Buffer.add_char buffer '\n';
      Buffer.contents buffer
    ) messages in
    push_to_msg_stream (Some formatted_messages);
    Lwt.return_unit
  in

  (* Just close the log *)
  let close () = Option.value_map ~default:Lwt.return_unit ~f:Lwt_unix.close log_fd in

  (* Set the default logger *)
  Lwt_log.default := Lwt_log_core.make ~output ~close;

  (* Set the min level *)
  Lwt_log_core.add_rule "*" min_level

let fatal = Lwt_log_core.ign_fatal_f
let error = Lwt_log_core.ign_error_f
let warn = Lwt_log_core.ign_warning_f
let info = Lwt_log_core.ign_info_f
let debug = Lwt_log_core.ign_debug_f
