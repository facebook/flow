(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val blocking_waitpid : int -> (int * Unix.process_status) Lwt.t

val exec_read : string -> string list -> string Lwt.t

type command_result = {
  stdout: string;
  stderr: string;
  status: Unix.process_status;
}

val exec : string -> string list -> command_result Lwt.t
