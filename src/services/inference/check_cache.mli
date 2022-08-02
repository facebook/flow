(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val create : capacity:int -> t

val find_or_create :
  t ->
  leader:File_key.t Lazy.t ->
  master_cx:Context.master_context ->
  create_file:(Context.component_t -> Type_sig_merge.file) ->
  File_key.t ->
  Type_sig_merge.file

val clear : t -> unit
