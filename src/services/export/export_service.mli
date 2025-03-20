(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init :
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  libs:Exports.t * ('a * Exports.t) list ->
  Utils_js.FilenameSet.t ->
  Export_search.t Lwt.t

val update :
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  dirty_files:Utils_js.FilenameSet.t ->
  Export_search.t ->
  Export_search.t Lwt.t

module For_test : sig
  val inferred_name_of_modulename : string -> string
end
