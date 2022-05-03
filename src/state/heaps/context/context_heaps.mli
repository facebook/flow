(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type READER = sig
  type reader

  val find_master : reader:reader -> Context.master_context
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

module Init_master_context_mutator : sig
  val add_master : (Context.master_context -> unit) Expensive.t
end
