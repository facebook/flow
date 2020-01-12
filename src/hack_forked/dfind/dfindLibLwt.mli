(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val init : Unix.file_descr * Unix.file_descr * Unix.file_descr -> string * Path.t list -> t

val wait_until_ready : t -> unit Lwt.t

val pid : t -> int

val get_changes : t -> SSet.t Lwt.t

val stop : t -> unit
