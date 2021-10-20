(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L : Loc_sig.S

  type info

  val empty : info

  val is_provider : info -> L.t -> bool

  val get_providers_for_toplevel_var : string -> info -> L.LSet.t option

  val find_providers : (L.t, L.t) Flow_ast.Program.t -> info

  val print_full_env : info -> string

  val providers_of_def :
    info ->
    L.t ->
    ( bool (* Is the def fully-initialized, e.g. not null- or un-initialized *)
    * L.t Reason.virtual_reason list
    )
    option

  val is_provider_of_annotated : info -> L.t -> bool
end

module Make (L : Loc_sig.S) : S with module L = L = struct
  module L = L
  module FP = Find_providers.FindProviders (L)
  open FP

  type info = {
    all_entries: EntrySet.t;
    all_exact_providers: L.LSet.t;
    all_annotated_providers: L.LSet.t;
    raw_env: env;
  }

  let empty =
    {
      all_entries = EntrySet.empty;
      all_exact_providers = L.LSet.empty;
      all_annotated_providers = L.LSet.empty;
      raw_env = empty_env;
    }

  let all_exact_providers entries =
    EntrySet.fold (fun { provider_locs; _ } -> L.LSet.union provider_locs) entries L.LSet.empty

  let is_provider { all_exact_providers; _ } loc = L.LSet.mem loc all_exact_providers

  let all_annotated_providers entries =
    EntrySet.fold
      (function
        | { state = AnnotatedVar; provider_locs; _ } -> (fun acc -> L.LSet.union provider_locs acc)
        | _ -> (fun acc -> acc))
      entries
      L.LSet.empty

  let is_provider_of_annotated { all_annotated_providers; _ } loc =
    L.LSet.mem loc all_annotated_providers

  let providers_of_def { all_entries; _ } loc =
    Base.List.find_map
      ~f:(fun { declare_locs; def_locs; provider_locs; name; state; _ } ->
        if L.LSet.mem loc declare_locs || L.LSet.mem loc def_locs then
          let fully_initialized =
            match state with
            | InitializedVar
            | AnnotatedVar ->
              true
            | UninitializedVar
            | NullInitializedVar ->
              false
          in
          let provider_reasons =
            L.LSet.elements provider_locs
            |> Base.List.map ~f:Reason.(mk_reason (RIdentifier (OrdinaryName name)))
          in
          Some (fully_initialized, provider_reasons)
        else
          None)
      (EntrySet.elements all_entries)

  let get_providers_for_toplevel_var var { raw_env; _ } = get_providers_for_toplevel_var var raw_env

  let find_providers (_, program) =
    let env = compute_provider_env program in
    let all_entries = all_entries env in
    {
      all_entries;
      all_exact_providers = all_exact_providers all_entries;
      all_annotated_providers = all_annotated_providers all_entries;
      raw_env = env;
    }

  let print_full_env { raw_env; _ } = print_full_env raw_env
end

module LocProviders = Make (Loc_sig.LocS)
module ALocProviders = Make (Loc_sig.ALocS)
include ALocProviders
