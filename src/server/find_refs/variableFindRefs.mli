(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_refs:
  ServerEnv.genv ->
  ServerEnv.env ref ->
  File_key.t ->
  content: string ->
  Loc.t ->
  global: bool ->
  ((string * Loc.t list * int option) option, string) result Lwt.t
