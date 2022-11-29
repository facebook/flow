(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception ImpossibleState of string

(* This describes the state of a variable AFTER the provider analysis, suitable for external consumption *)
type state =
  | AnnotatedVar of {
      contextual: bool;
      predicate: bool; (* true iff this annotation corresponds to a predicate function (%checks) *)
    }
  | InitializedVar
  | ArrayInitializedVar
  | EmptyArrayInitializedVar
  | NullInitializedVar
  | UninitializedVar

type write_kind =
  | EmptyArray
  | Ordinary

module FindProviders (L : Loc_sig.S) : sig
  module Id : sig
    type t
  end

  type ('locs, 'state) base_entry = {
    entry_id: Id.t;
    name: string;
    state: 'state;
    declare_locs: L.LSet.t;
    def_locs: L.LSet.t;
    provider_locs: 'locs;
    binding_kind: Bindings.kind;
  }

  type providers = {
    writes: write_kind L.LMap.t;
    array_writes: L.LSet.t;
  }

  type entry = (providers, state) base_entry

  type env

  module EntrySet : Flow_set.S with type elt = entry

  val empty_env : env

  val compute_provider_env : (L.t, L.t) Flow_ast.Program.t' -> env

  val all_entries : env -> EntrySet.t

  val get_providers_for_toplevel_var : string -> env -> write_kind L.LMap.t option
end
