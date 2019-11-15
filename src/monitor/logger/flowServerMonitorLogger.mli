(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init_logger : Unix.file_descr option -> unit

type 'a logger_fn = ?exn:exn -> ('a, unit, string, unit) format4 -> 'a

type 'a logger_fn_s = ?exn:Exception.t -> ('a, unit, string, unit) format4 -> 'a

(* Async APIs *)
val fatal : 'a logger_fn

val error : 'a logger_fn

val warn : 'a logger_fn

val info : 'a logger_fn

val debug : 'a logger_fn

(* Sync APIs *)
val fatal_s : 'a logger_fn_s

val error_s : 'a logger_fn_s

val warn_s : 'a logger_fn_s

val info_s : 'a logger_fn_s

val debug_s : 'a logger_fn_s
