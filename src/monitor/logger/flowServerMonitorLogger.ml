(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
 * *)

type 'a logger_fn = ?exn:exn -> ('a, unit, string, unit) format4 -> 'a

type 'a logger_fn_s = ?exn:Exception.t -> ('a, unit, string, unit) format4 -> 'a

let (msg_stream, push_to_msg_stream) = Lwt_stream.create ()

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
    Printf.eprintf "Logger.WriteLoop exception:\n%s" (Exception.to_string exn);
    Exception.reraise exn
end)

let initialized = ref false

(* We're using lwt's logger instead of Hh_logger, so let's map Hh_logger levels to lwt levels *)
let lwt_level_of_hh_logger_level = function
  | Hh_logger.Level.Off -> Lwt_log_core.Fatal
  | Hh_logger.Level.Fatal -> Lwt_log_core.Fatal
  | Hh_logger.Level.Error -> Lwt_log_core.Error
  | Hh_logger.Level.Warn -> Lwt_log_core.Warning
  | Hh_logger.Level.Info -> Lwt_log_core.Info
  | Hh_logger.Level.Debug -> Lwt_log_core.Debug

(* Creates a default logger and sets the minimum logger level. The logger will log every message
 * that passes the minimum level to stderr. If log_fd is provided, each message will be logged
 * to it as well *)
let init_logger log_fd =
  if !initialized then failwith "Cannot initialized FlowServerMonitorLogger more than once";
  initialized := true;

  let min_level = Hh_logger.Level.min_level () |> lwt_level_of_hh_logger_level in
  let template = "$(date).$(milliseconds) [$(level)] $(message)" in
  let log_fd = Option.map log_fd ~f:(Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true) in
  let fds = Lwt_unix.stderr :: Option.value_map log_fd ~default:[] ~f:(fun fd -> [fd]) in
  Lwt.async (fun () -> WriteLoop.run fds);

  (* Format the messages and write the to the log and stderr *)
  let output section level messages =
    let buffer = Buffer.create 42 in
    let formatted_messages =
      Base.List.map
        ~f:(fun message ->
          Buffer.clear buffer;
          Lwt_log.render ~buffer ~template ~section ~level ~message;
          Buffer.add_char buffer '\n';
          Buffer.contents buffer)
        messages
    in
    push_to_msg_stream (Some formatted_messages);
    Lwt.return_unit
  in
  (* Just close the log *)
  let close () = Option.value_map ~default:Lwt.return_unit ~f:Lwt_unix.close log_fd in
  (* Set the default logger *)
  Lwt_log.default := Lwt_log_core.make ~output ~close;

  (* Set the min level *)
  Lwt_log_core.add_rule "*" min_level

(* Async logging APIs. These are the APIs you should generally use. Since they're async, they
 * won't make the monitor unresponsive while they're logging *)
let fatal ?exn fmt = Lwt_log_core.ign_fatal_f ?exn fmt

let error ?exn fmt = Lwt_log_core.ign_error_f ?exn fmt

let warn ?exn fmt = Lwt_log_core.ign_warning_f ?exn fmt

let info ?exn fmt = Lwt_log_core.ign_info_f ?exn fmt

let debug ?exn fmt = Lwt_log_core.ign_debug_f ?exn fmt

(* Synchronous versions just delegate to Hh_logger. These are mainly used for debugging, when you
 * want a logging call to write to the log RIGHT NOW. *)
let fatal_s = Hh_logger.fatal

let error_s = Hh_logger.error

let warn_s = Hh_logger.warn

let info_s = Hh_logger.info

let debug_s = Hh_logger.debug
