(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections

module type ConvertLoc = sig
  module L : Loc_sig.S

  val convert : L.t -> Loc.t
end

module Make
    (L : Loc_sig.S)
    (Env_builder : Env_builder.S with module L = L)
    (Provider_api : Provider_api.S with module L = L)
    (Convert : ConvertLoc with module L = L) =
struct
  class use_finder env providers =
    object (this)
      inherit [LocSet.t, L.t] Flow_ast_visitor.visitor ~init:LocSet.empty as super

      method update_convert_acc set =
        this#update_acc (L.LSet.fold (fun elt acc -> LocSet.add (Convert.convert elt) acc) set)

      method! identifier ((loc, _) as id) =
        this#update_convert_acc (Env_builder.refiners_of_use env loc);
        id

      method! pattern_identifier ?kind ((loc, _) as id) =
        ignore kind;
        if not @@ Provider_api.is_provider providers loc then
          this#update_convert_acc
            (Base.Option.value_exn (Provider_api.providers_of_def providers loc));
        id

      method! statement ((loc, _) as stmt) =
        let stmt = super#statement stmt in
        this#update_acc (LocSet.filter (Fn.compose not (Loc.contains (Convert.convert loc))));
        stmt
    end

  let uses_of_statement env providers stmt =
    let finder = new use_finder env providers in
    finder#eval finder#statement stmt

  let calc_index_deps env providers statements =
    let enum_statements = Base.List.mapi ~f:(fun i s -> (i, s)) statements in
    let stmt_deps_locs =
      Base.List.map ~f:(fun (i, s) -> (i, uses_of_statement env providers s)) enum_statements
    in
    Base.List.map stmt_deps_locs ~f:(fun (i, locs) ->
        ( i,
          Base.List.fold enum_statements ~init:ISet.empty ~f:(fun acc (j, (loc, _)) ->
              if LocSet.exists (Loc.contains (Convert.convert loc)) locs then
                ISet.add j acc
              else
                acc) ))
end

module With_Loc =
  Make (Loc_sig.LocS) (Env_builder.With_Loc) (Provider_api.LocProviders)
    (struct
      module L = Loc_sig.LocS

      let convert loc = loc
    end)
