(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type kind =
  | Var
  | Let
  | ThisAnnot
  | Const
  | DeclaredVar
  | DeclaredLet
  | DeclaredConst
  | Type of {
      imported: bool;
      type_only_namespace: bool;
    }
  | Interface of {
      imported: bool;
      type_only_namespace: bool;
    }
  | Enum
  | Function
  | Class
  | DeclaredClass
  | DeclaredNamespace
  | Parameter
  | CatchParameter
  | Import
  | TsImport
  | DeclaredFunction
  | Internal
  | GeneratorNext
  | Component
  | ComponentParameter
  | Record
[@@deriving show]

type 'loc entry = ('loc, 'loc) Ast.Identifier.t * kind

type 'loc t = 'loc entry list

let empty = []

let singleton x = [x]

let add = List.cons

let push = List.append

let exists = List.exists

let to_assoc t =
  let (xs, map) =
    List.fold_left
      (fun (xs, map) ((loc, { Ast.Identifier.name = x; comments = _ }), kind) ->
        match SMap.find_opt x map with
        (* First kind wins *)
        | Some (kind, locs) -> (xs, SMap.add x (kind, Nel.cons loc locs) map)
        | None -> (x :: xs, SMap.add x (kind, Nel.one loc) map))
      ([], SMap.empty)
      (List.rev t)
  in
  List.rev_map (fun x -> (x, SMap.find x map |> fun (kind, locs) -> (kind, Nel.rev locs))) xs

let to_map t =
  let map =
    List.fold_left
      (fun map ((loc, { Ast.Identifier.name = x; comments = _ }), kind) ->
        match SMap.find_opt x map with
        | Some (canonical_kind, entries) ->
          SMap.add x (canonical_kind, Nel.cons (loc, kind) entries) map
        | None -> SMap.add x (kind, Nel.one (loc, kind)) map)
      SMap.empty
      (List.rev t)
  in
  SMap.map (fun (canonical_kind, entries) -> (canonical_kind, Nel.rev entries)) map

let allow_forward_ref = function
  | DeclaredFunction
  | DeclaredClass
  | DeclaredVar
  | DeclaredLet
  | DeclaredConst
  | DeclaredNamespace
  | Var
  | Function
  | Component
  | Interface _ ->
    true
  | _ -> false

let allow_redeclaration = function
  | DeclaredFunction
  | Var
  | Parameter
  | Function ->
    true
  | _ -> false
