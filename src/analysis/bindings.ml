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
  | Enum
  | Function
  | Class
  | DeclaredClass
  | DeclaredNamespace
  | Parameter
  | CatchParameter
  | Import
  | DeclaredFunction of { predicate: bool }
  | Internal
  | GeneratorNext
  | Component
  | ComponentParameter
[@@deriving show]

type 'loc entry = ('loc, 'loc) Ast.Identifier.t * kind

type 'loc t = 'loc entry list

let empty = []

let singleton x = [x]

let add = List.cons

let push = List.append

let exists = List.exists

let to_assoc t =
  let rec loop acc = function
    | [] -> acc
    | ((loc, { Ast.Identifier.name = x; comments = _ }), kind) :: rest ->
        let (xs, map) = acc in
        match SMap.find_opt x map with
        | Some (kind, locs) -> loop (xs, SMap.add x (kind, Nel.cons loc locs) map) rest
        | None -> loop (x :: xs, SMap.add x (kind, Nel.one loc) map) rest
  in
  let (xs, map) = loop ([], SMap.empty) (List.rev t) in
  List.rev_map (fun x -> (x, SMap.find x map |> fun (kind, locs) -> (kind, Nel.rev locs))) xs

let to_map t =
  let map =
    List.fold_left
      (fun map ((loc, { Ast.Identifier.name = x; comments = _ }), kind) ->
        match SMap.find_opt x map with
        | Some (kind, locs) -> SMap.add x (kind, Nel.cons loc locs) map
        | None -> SMap.add x (kind, Nel.one loc) map)
      SMap.empty
      (List.rev t)
  in
  SMap.map (fun (kind, locs) -> (kind, Nel.rev locs)) map

let allow_forward_ref = function
  | DeclaredFunction _
  | DeclaredClass
  | DeclaredVar
  | DeclaredLet
  | DeclaredConst
  | DeclaredNamespace
  | Var
  | Function
  | Component ->
    true
  | _ -> false

let allow_redeclaration = function
  | DeclaredFunction _
  | Var
  | Parameter
  | Function ->
    true
  | _ -> false
