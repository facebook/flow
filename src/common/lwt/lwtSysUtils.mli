(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val blocking_waitpid : int -> (int * Unix.process_status) Lwt.t

type command_result = {
  stdout: string;
  stderr: string;
  status: Unix.process_status;
}

val exec : ?env:string array -> ?cwd:string -> string -> string list -> command_result Lwt.t

val exec_with_timeout :
  timeout:float -> string -> string list -> (command_result, string) result Lwt.t
