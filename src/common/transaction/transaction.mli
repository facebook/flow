(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val add :
  ?singleton:string -> commit:(unit -> unit Lwt.t) -> rollback:(unit -> unit Lwt.t) -> t -> unit

val with_transaction : (t -> 'a Lwt.t) -> 'a Lwt.t
