(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init_logger: ?log_fd:Unix.file_descr -> Lwt_log_core.level -> unit

type 'a logger_fn =
  ?exn : exn ->
  ?section : Lwt_log_core.section ->
  ?location : (string * int * int) ->
  ?logger:Lwt_log_core.logger ->
  ('a, unit, string, unit) format4 ->
  'a

val fatal: 'a logger_fn
val error: 'a logger_fn
val warn: 'a logger_fn
val info: 'a logger_fn
val debug: 'a logger_fn
