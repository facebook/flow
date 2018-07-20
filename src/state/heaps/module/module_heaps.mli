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

val get_resolved_requires_unsafe: (File_key.t -> resolved_requires) Expensive.t

type info = {
  module_name: Modulename.t;
  checked: bool;            (* in flow? *)
  parsed: bool;             (* if false, it's a tracking record only *)
}

(* given a filename, returns module info *)
val get_info_unsafe: (File_key.t -> info) Expensive.t
val get_info: (File_key.t -> info option) Expensive.t
val is_tracked_file: File_key.t -> bool

(* WILL DELETE SOON *)
val add_info: (File_key.t -> info -> unit) Expensive.t
val info_heap_clear_batch: Utils_js.FilenameSet.t -> unit

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

module Resolved_requires_mutator : sig
  type t
  val create: Transaction.t -> Utils_js.FilenameSet.t -> t
  val add_resolved_requires: t -> File_key.t -> resolved_requires -> unit
end

module FromSavedState : sig
  val add_resolved_requires: File_key.t -> resolved_requires -> unit
end
