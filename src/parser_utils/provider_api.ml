(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module type S = sig
  module L : Loc_sig.S

  type info

  val empty : info

  val is_provider : info -> L.t -> bool

  val get_providers_for_toplevel_var : string -> info -> L.LSet.t option

  val find_providers : (L.t, L.t) Flow_ast.Program.t -> info

  val print_full_env : info -> string

  val providers_of_def : info -> L.t -> L.t Reason.virtual_reason list option

  val is_provider_of_annotated : info -> L.t -> bool
end

module Make (L : Loc_sig.S) : S with module L = L = struct
  module L = L
  module Find_providers = Find_providers.FindProviders (L)
  open Find_providers

  module EntrySet = Flow_set.Make (struct
    type t = entry

    let compare { entry_id = id1; _ } { entry_id = id2; _ } = Id.compare id1 id2
  end)

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
      raw_env = Nel.one (new_scope ~kind:Var);
    }

  let all_exact_providers entries =
    EntrySet.fold (fun { provider_locs; _ } -> L.LSet.union provider_locs) entries L.LSet.empty

  let rec all_entries { entries; children; _ } =
    L.LMap.fold (fun _ child acc -> EntrySet.union (all_entries child) acc) children EntrySet.empty
    |> SMap.fold (fun _ entry acc -> EntrySet.add entry acc) entries

  let is_provider { all_exact_providers; _ } loc = L.LSet.mem loc all_exact_providers

  let all_annotated_providers entries =
    EntrySet.fold
      (function
        | { state = Find_providers.Annotated; provider_locs; _ } ->
          (fun acc -> L.LSet.union provider_locs acc)
        | _ -> (fun acc -> acc))
      entries
      L.LSet.empty

  let is_provider_of_annotated { all_annotated_providers; _ } loc =
    L.LSet.mem loc all_annotated_providers

  let providers_of_def { all_entries; _ } loc =
    Base.List.find_map
      ~f:(fun { declare_locs; def_locs; provider_locs; name; _ } ->
        if L.LSet.mem loc declare_locs || L.LSet.mem loc def_locs then
          Some
            (L.LSet.elements provider_locs
            |> Base.List.map ~f:Reason.(mk_reason (RIdentifier (OrdinaryName name))))
        else
          None)
      (EntrySet.elements all_entries)

  let get_providers_for_toplevel_var var { raw_env = ({ entries; _ }, _); _ } =
    let entry = SMap.find_opt var entries in
    Base.Option.map ~f:(fun { provider_locs; _ } -> provider_locs) entry

  let find_providers (_, program) =
    let env = find_declaration_statements program in
    let ((hd, _) as env) = find_provider_statements env program in
    let all_entries = all_entries hd in
    {
      all_entries;
      all_exact_providers = all_exact_providers all_entries;
      all_annotated_providers = all_annotated_providers all_entries;
      raw_env = env;
    }

  let print_full_env { raw_env = env; _ } =
    let rec ptabs count =
      if count = 0 then
        ""
      else
        spf " %s" (ptabs (count - 1))
    in
    let rec print_rec label tabs { providers; entries = _; children; _ } =
      let msg = spf "%s%s:\n" (ptabs tabs) label in
      let tabs = tabs + 1 in
      let t = ptabs tabs in
      let msg =
        spf
          "%s%sproviders: \n%s\n"
          msg
          t
          (SMap.bindings providers
          |> Base.List.map ~f:(fun (k, { relative_locs; exact_locs }) ->
                 spf
                   "%s %s:\n%s  relative: (%s)\n%s  exact: (%s)"
                   t
                   k
                   t
                   (L.LSet.elements relative_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), (")
                   t
                   (L.LSet.elements exact_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), ("))
          |> String.concat "\n")
      in
      spf
        "%s%schildren:\n%s"
        msg
        t
        (L.LMap.bindings children
        |> Base.List.map ~f:(fun (loc, scope) ->
               print_rec (L.debug_to_string ~include_source:false loc) (tabs + 1) scope)
        |> String.concat "\n")
    in
    match env with
    | (top, []) -> print_rec "toplevel" 0 top
    | _ -> env_invariant_violated "Final environment has depth =/= 1"
end

module LocProviders = Make (Loc_sig.LocS)
module ALocProviders = Make (Loc_sig.ALocS)
include ALocProviders
