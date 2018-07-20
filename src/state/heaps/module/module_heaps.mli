(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val get_file: (Modulename.t -> File_key.t option) Expensive.t
val get_file_unsafe: (Modulename.t -> File_key.t) Expensive.t
val module_exists: Modulename.t -> bool

type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
}

(*** WILL DELETE SOON ***)
val add_resolved_requires: (File_key.t -> resolved_requires -> unit) Expensive.t
val remove_batch_resolved_requires: Utils_js.FilenameSet.t -> unit
(************************)

val get_resolved_requires_unsafe: (File_key.t -> resolved_requires) Expensive.t

module Commit_modules_mutator : sig
  type t
  val create: Transaction.t -> is_init:bool -> t
  val remove_and_replace:
    t ->
    workers:MultiWorkerLwt.worker list option ->
    to_remove:Modulename.Set.t ->
    to_replace:(Modulename.t *  File_key.t) list ->
    unit Lwt.t
end

module FromSavedState : sig
  val add_resolved_requires: File_key.t -> resolved_requires -> unit
end
