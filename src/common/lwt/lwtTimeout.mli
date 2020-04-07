(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val with_timeout :
  ?timeout_msg:string ->
  ?on_timeout:(unit -> unit Lwt.t) ->
  float ->
  (unit -> ('a, string) result Lwt.t) ->
  ('a, string) result Lwt.t
