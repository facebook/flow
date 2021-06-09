(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create : capacity:int -> t

val find_or_create :
  t ->
  find_leader:(File_key.t -> File_key.t) ->
  master_cx:Context.master_context ->
  create_file:(File_key.t -> Context.component_t -> Type_sig_merge.file) ->
  File_key.t ->
  Type_sig_merge.file

val remove : t -> File_key.t -> unit

val clear : t -> unit
