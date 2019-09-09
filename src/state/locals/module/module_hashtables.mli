(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type READER = sig
  type reader

  val find_in_all_providers_unsafe : reader:reader -> Modulename.t -> Utils_js.FilenameSet.t
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t

module All_providers_mutator : sig
  type t

  val create : Transaction.t -> t

  val add_provider : t -> File_key.t -> Modulename.t -> unit

  val remove_provider : t -> File_key.t -> Modulename.t -> unit
end

val memoize_with_module_name_candidates_cache : f:(string -> string list) -> string -> string list
