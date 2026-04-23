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

type namespace =
  | NValue
  | NType
[@@deriving show]

let namespaces_of_kind = function
  | Type _
  | Interface _ ->
    [NType]
  | Class
  | DeclaredClass
  | Enum
  | DeclaredNamespace ->
    [NValue; NType]
  | _ -> [NValue]

let same_namespace k1 k2 =
  let ns1 = namespaces_of_kind k1 in
  let ns2 = namespaces_of_kind k2 in
  List.exists (fun n -> List.mem n ns2) ns1

(* Bindings.t is stored newest-first (entries are added with List.cons).
   split_by_namespace preserves that ordering within each output list. *)
let split_by_namespace t =
  List.fold_left
    (fun (vs, ts) ((_, kind) as entry) ->
      let nss = namespaces_of_kind kind in
      let vs =
        if List.mem NValue nss then
          entry :: vs
        else
          vs
      in
      let ts =
        if List.mem NType nss then
          entry :: ts
        else
          ts
      in
      (vs, ts))
    ([], [])
    (List.rev t)
