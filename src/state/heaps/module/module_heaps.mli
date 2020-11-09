(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
  hash: Xx.hash;
}

val mk_resolved_requires :
  resolved_modules:Modulename.t SMap.t -> phantom_dependents:SSet.t -> resolved_requires

type info = {
  module_name: Modulename.t;
  checked: bool;
  (* in flow? *)
  parsed: bool; (* if false, it's a tracking record only *)
}

module type READER = sig
  type reader

  val get_file : reader:reader -> (Modulename.t -> File_key.t option) Expensive.t

  val get_file_unsafe : reader:reader -> (Modulename.t -> File_key.t) Expensive.t

  val module_exists : reader:reader -> Modulename.t -> bool

  val get_resolved_requires_unsafe : reader:reader -> (File_key.t -> resolved_requires) Expensive.t

  (* given a filename, returns module info *)
  val get_info_unsafe : reader:reader -> (File_key.t -> info) Expensive.t

  val get_info : reader:reader -> (File_key.t -> info option) Expensive.t

  val is_tracked_file : reader:reader -> File_key.t -> bool
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

module Commit_modules_mutator : sig
  type t

  val create : Transaction.t -> is_init:bool -> t

  val remove_and_replace :
    t ->
    workers:MultiWorkerLwt.worker list option ->
    to_remove:Modulename.Set.t ->
    to_replace:(Modulename.t * File_key.t) list ->
    unit Lwt.t
end

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires : t -> File_key.t -> resolved_requires -> bool
end

module Introduce_files_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_info : t -> File_key.t -> info -> unit
end

module From_saved_state : sig
  val add_resolved_requires : File_key.t -> resolved_requires -> unit
end
