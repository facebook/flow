(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type READER = sig
  type reader

  val get_package : reader:reader -> string -> (Package_json.t, unit) result option

  val get_package_directory : reader:reader -> string -> string option
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

module Package_heap_mutator : sig
  val add_package_json : string -> Package_json.t -> unit

  val add_error : string -> unit
end

module For_saved_state : sig
  val get_package_json_unsafe : string -> (Package_json.t, unit) result
end
