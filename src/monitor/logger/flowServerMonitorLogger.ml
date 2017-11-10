(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module makes Lwt's logger easily available for use. Lwt's logger is nice because it
 * can easily log exceptions, format messages, and interact with Lwt. *)

let (>>=) = Lwt.(>>=)

type 'a logger_fn =
  ?exn : exn ->
  ?section : Lwt_log_core.section ->
  ?location : (string * int * int) ->
  ?logger:Lwt_log_core.logger ->
  ('a, unit, string, unit) format4 ->
  'a

(* Creates a default logger and sets the minimum logger level. The logger will log every message
 * that passes the minimum level to stderr. If log_fd is provided, each message will be logged
 * to it as well *)
let init_logger ?log_fd min_level =
  let template = "$(date).$(milliseconds) [$(level)] $(message)" in
  let log_oc = Option.map ~f:(Lwt_io.of_unix_fd ~mode:Lwt_io.output) log_fd in

  (* Given a list of formatted messages, atomically write them to the channel and flush it *)
  let atomic_write_list formatted_messages =
    Lwt_io.atomic begin fun oc ->
      Lwt_list.iter_s (Lwt_io.write oc) formatted_messages
      >>= fun () -> Lwt_io.flush oc
    end
  in

  (* Format the messages and write the to the log and stderr *)
  let output section level messages =
    let buffer = Buffer.create 42 in
    let formatted_messages = List.map (fun message ->
      Buffer.clear buffer;
      Lwt_log.render ~buffer ~template ~section ~level ~message;
      Buffer.add_char buffer '\n';
      Buffer.contents buffer
    ) messages in
    Lwt.join [
      atomic_write_list formatted_messages Lwt_io.stderr;
      Option.value_map ~default:Lwt.return_unit ~f:(atomic_write_list formatted_messages) log_oc;
    ]
  in

  (* Just close the log *)
  let close () = Option.value_map ~default:Lwt.return_unit ~f:Lwt_io.close log_oc in

  (* Set the default logger *)
  Lwt_log.default := Lwt_log_core.make ~output ~close;

  (* Set the min level *)
  Lwt_log_core.add_rule "*" min_level

let fatal = Lwt_log_core.ign_fatal_f
let error = Lwt_log_core.ign_error_f
let warn = Lwt_log_core.ign_warning_f
let info = Lwt_log_core.ign_info_f
let debug = Lwt_log_core.ign_debug_f
