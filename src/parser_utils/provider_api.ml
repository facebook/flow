(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module type S = sig
  module L : Loc_sig.S

  type scope

  type env = scope Nel.t

  val empty_scope : scope

  val is_provider : scope -> L.t -> bool

  val get_providers_for_toplevel_var : string -> env -> L.LSet.t option

  val find_providers : (L.t, L.t) Flow_ast.Program.t -> env

  val print_full_env : env -> string

  val providers_of_def : scope -> L.t -> L.LSet.t option
end

module Make (L : Loc_sig.S) = struct
  module L = L
  module Find_providers = Find_providers.FindProviders (L)
  open Find_providers

  type scope = Find_providers.scope

  type env = Find_providers.env

  let empty_scope = new_scope ~kind:Var

  let all_exact_providers { providers; _ } =
    SMap.fold (fun _ { exact_locs; _ } acc -> L.LSet.union exact_locs acc) providers L.LSet.empty

  let rec all_entries { entries; children; _ } =
    SMap.values entries @ Base.List.concat_map ~f:all_entries (L.LMap.values children)

  let is_provider scope loc = L.LSet.mem loc (all_exact_providers scope)

  let providers_of_def scope loc =
    let all_entries = all_entries scope in
    Base.List.find_map
      ~f:(fun { provider_locs; declare_locs; def_locs; _ } ->
        if L.LSet.mem loc declare_locs || L.LSet.mem loc def_locs then
          Some provider_locs
        else
          None)
      all_entries

  let get_providers_for_toplevel_var var ({ entries; _ }, _) =
    let entry = SMap.find_opt var entries in
    Base.Option.map ~f:(fun { provider_locs; _ } -> provider_locs) entry

  let find_providers (_, program) =
    let env = find_declaration_statements program in
    find_provider_statements env program

  let print_full_env env =
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
          ( SMap.bindings providers
          |> Base.List.map ~f:(fun (k, { relative_locs; exact_locs }) ->
                 spf
                   "%s %s:\n%s  relative: (%s)\n%s  exact: (%s)"
                   t
                   k
                   t
                   ( L.LSet.elements relative_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), (" )
                   t
                   ( L.LSet.elements exact_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), (" ))
          |> String.concat "\n" )
      in
      spf
        "%s%schildren:\n%s"
        msg
        t
        ( L.LMap.bindings children
        |> Base.List.map ~f:(fun (loc, scope) ->
               print_rec (L.debug_to_string ~include_source:false loc) (tabs + 1) scope)
        |> String.concat "\n" )
    in
    match env with
    | (top, []) -> print_rec "toplevel" 0 top
    | _ -> env_invariant_violated "Final environment has depth =/= 1"
end

module LocProviders = Make (Loc_sig.LocS)
module ALocProviders = Make (Loc_sig.ALocS)
include ALocProviders
