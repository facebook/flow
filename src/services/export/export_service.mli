(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init :
  index_star_exports:bool ->
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  libs:Exports.t ->
  Utils_js.FilenameSet.t ->
  Export_search.t Lwt.t

val update :
  index_star_exports:bool ->
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  dirty_files:Utils_js.FilenameSet.t ->
  Export_search.t ->
  Export_search.t Lwt.t

module For_test : sig
  val string_of_modulename : Modulename.t -> string
end
