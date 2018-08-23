(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val blocking_waitpid: int -> (int * Unix.process_status) Lwt.t

val exec_read: string -> string list -> string Lwt.t
