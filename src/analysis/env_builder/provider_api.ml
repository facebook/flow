(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module type S = sig
  module L : Loc_sig.S

  type info

  type provider = {
    reason: L.t virtual_reason;
    empty_array_writes: L.LSet.t option;
  }

  type def_providers = {
    state: Find_providers.state;
    providers: provider list;
    array_providers: L.LSet.t;
  }

  val empty : info

  val is_provider : info -> L.t -> bool

  val is_array_provider : info -> L.t -> bool

  val get_providers_for_toplevel_var : string -> info -> Find_providers.write_kind L.LMap.t option

  val find_providers : (L.t, L.t) Flow_ast.Program.t -> info

  val providers_of_def : info -> L.t -> def_providers option

  val is_provider_of_annotated : info -> L.t -> bool

  val is_provider_state_fully_initialized : Find_providers.state -> bool
end

module Make (L : Loc_sig.S) : S with module L = L = struct
  module L = L
  module FP = Find_providers.FindProviders (L)
  open FP

  type provider = {
    reason: L.t virtual_reason;
    empty_array_writes: L.LSet.t option;
  }

  type def_providers = {
    state: Find_providers.state;
    providers: provider list;
    array_providers: L.LSet.t;
  }

  type info = {
    all_exact_providers: L.LSet.t;
    all_array_providers: L.LSet.t;
    all_annotated_providers: L.LSet.t;
    all_providers_of_writes: def_providers L.LMap.t;
    raw_env: env;
  }

  let empty =
    {
      all_exact_providers = L.LSet.empty;
      all_array_providers = L.LSet.empty;
      all_annotated_providers = L.LSet.empty;
      all_providers_of_writes = L.LMap.empty;
      raw_env = empty_env;
    }

  let all_exact_providers entries =
    EntrySet.fold
      (fun { provider_locs; _ } -> L.LMap.fold (fun loc _ -> L.LSet.add loc) provider_locs.writes)
      entries
      L.LSet.empty

  let all_array_providers entries =
    EntrySet.fold
      (fun { provider_locs; _ } -> L.LSet.union provider_locs.array_writes)
      entries
      L.LSet.empty

  let is_provider { all_exact_providers; _ } loc = L.LSet.mem loc all_exact_providers

  let is_array_provider { all_array_providers; _ } loc = L.LSet.mem loc all_array_providers

  let all_annotated_providers entries =
    EntrySet.fold
      (function
        | { state = Find_providers.AnnotatedVar _; provider_locs; _ } ->
          L.LMap.fold (fun loc _ -> L.LSet.add loc) provider_locs.writes
        | _ -> (fun acc -> acc))
      entries
      L.LSet.empty

  let is_provider_of_annotated { all_annotated_providers; _ } loc =
    L.LSet.mem loc all_annotated_providers

  let is_provider_state_fully_initialized =
    Find_providers.(
      function
      | InitializedVar
      | AnnotatedVar _
      | ArrayInitializedVar
      | EmptyArrayInitializedVar ->
        true
      | UninitializedVar
      | NullInitializedVar ->
        false
    )

  let providers_of_def { all_providers_of_writes; _ } loc =
    L.LMap.find_opt loc all_providers_of_writes

  let all_providers_of_writes set =
    EntrySet.fold
      (fun { declare_locs; def_locs; provider_locs; name; state; _ } ->
        let providers =
          L.LMap.fold
            (fun loc write_kind acc ->
              let reason = Reason.(mk_reason (RIdentifier (OrdinaryName name)) loc) in
              let empty_array_writes =
                let open Find_providers in
                match write_kind with
                | Ordinary -> None
                | EmptyArray -> Some provider_locs.array_writes
              in
              { reason; empty_array_writes } :: acc)
            provider_locs.writes
            []
          |> Base.List.sort ~compare:(fun { reason = r1; _ } { reason = r2; _ } ->
                 Reason.(L.compare (poly_loc_of_reason r1) (poly_loc_of_reason r2))
             )
        in
        L.LSet.fold
          (fun loc ->
            L.LMap.add loc { state; providers; array_providers = provider_locs.array_writes })
          (L.LSet.union def_locs declare_locs))
      set
      L.LMap.empty

  let get_providers_for_toplevel_var var { raw_env; _ } = get_providers_for_toplevel_var var raw_env

  let find_providers (_, program) =
    let env = compute_provider_env program in
    let all_entries = all_entries env in
    {
      all_exact_providers = all_exact_providers all_entries;
      all_array_providers = all_array_providers all_entries;
      all_annotated_providers = all_annotated_providers all_entries;
      all_providers_of_writes = all_providers_of_writes all_entries;
      raw_env = env;
    }
end

module LocProviders = Make (Loc_sig.LocS)
module ALocProviders = Make (Loc_sig.ALocS)
include ALocProviders
