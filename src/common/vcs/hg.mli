(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error_status = Vcs_utils.error_status =
  | Not_installed of { path: string }
  | Errored of string

val merge_base : ?cwd:string -> string -> string -> (string, error_status) result Lwt.t

val files_changed_since : ?cwd:string -> string -> (string list, error_status) result Lwt.t

val files_changed_since_mergebase_with :
  ?cwd:string -> string -> (string * string list, error_status) result Lwt.t
