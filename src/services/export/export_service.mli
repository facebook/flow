(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val init :
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  libs:Exports.t ->
  Utils_js.FilenameSet.t ->
  Export_search.t Lwt.t

val update :
  workers:MultiWorkerLwt.worker list option ->
  reader:Mutator_state_reader.t ->
  update:Utils_js.FilenameSet.t ->
  remove:Utils_js.FilenameSet.t ->
  Export_search.t ->
  Export_search.t Lwt.t

module For_test : sig
  val string_of_modulename : Modulename.t -> string
end
