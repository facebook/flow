(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type resolved_requires = {
  file_key: File_key.t;
  resolved_modules: Modulename.t SMap.t;
  phantom_dependents: SSet.t;
  hash: Xx.hash;
}
[@@deriving show]

val mk_resolved_requires :
  File_key.t ->
  resolved_modules:Modulename.t SMap.t ->
  phantom_dependents:SSet.t ->
  resolved_requires

module type READER = sig
  type reader

  val get_resolved_requires_unsafe : reader:reader -> (File_key.t -> resolved_requires) Expensive.t
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires : t -> File_key.t -> resolved_requires -> bool
end

module From_saved_state : sig
  val add_resolved_requires : File_key.t -> resolved_requires -> unit
end

val iter_resolved_requires : (resolved_requires -> unit) -> unit
