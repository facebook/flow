(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module LocMap = Loc_collections.LocMap
module LocSet = Loc_collections.LocSet

type shorthandKind =
  | Obj
  | Import

let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc

class searcher ~reader ~(targets : Loc_collections.LocSet.t) ~(result : shorthandKind LocMap.t ref)
  =
  object (_this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! import_named_specifier ~import_kind:_ specifier =
      let open Flow_ast.Statement.ImportDeclaration in
      let { local; remote = ((aloc, _), _); kind = _ } = specifier in
      (match local with
      | Some _ -> ()
      | None ->
        let loc = loc_of_aloc ~reader aloc in
        if LocSet.mem loc targets then result := LocMap.add loc Import !result);
      specifier

    method! pattern ?kind expr =
      let (_, patt) = expr in
      let _ =
        match patt with
        | Ast.Pattern.Object { Ast.Pattern.Object.properties; _ } ->
          List.iter
            (fun prop ->
              let open Ast.Pattern.Object in
              match prop with
              | Property (_, { Property.key; Property.shorthand; _ }) ->
                (match key with
                | Property.Identifier ((aloc, _), _) ->
                  let loc = loc_of_aloc ~reader aloc in
                  if shorthand && LocSet.mem loc targets then result := LocMap.add loc Obj !result
                | Property.Computed _
                | Property.Literal _ ->
                  ())
              | RestElement _ -> ())
            properties
        | Ast.Pattern.Identifier _
        | Ast.Pattern.Array _
        | Ast.Pattern.Expression _ ->
          ()
      in
      super#pattern ?kind expr

    method! object_property prop =
      let open Ast.Expression.Object.Property in
      let (_, prop') = prop in
      let _ =
        match prop' with
        | Init { key = Identifier id; shorthand; value = _ } ->
          let ((aloc, _), _) = id in
          let loc = loc_of_aloc ~reader aloc in
          if shorthand && LocSet.mem loc targets then result := LocMap.add loc Obj !result
        | _ -> ()
      in
      super#object_property prop
  end

(**
Given a list of property locations, creates a map from each of those locations to the property kind.
If the property is not a shorthand or named import, then the property will not be added to the map
*)
let search ~reader ~targets ast =
  let result = ref Loc_collections.LocMap.empty in
  let s = new searcher ~reader ~targets ~result in
  let _ = s#program ast in
  !result
