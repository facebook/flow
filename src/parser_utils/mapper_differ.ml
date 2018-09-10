(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

exception Combine_inconsistency

type node =
  | Statement of (Loc.t, Loc.t) Ast.Statement.t * (Loc.t, Loc.t) Ast.Statement.t
  | Expression of (Loc.t, Loc.t) Ast.Expression.t * (Loc.t, Loc.t) Ast.Expression.t
  | ClassElement of (Loc.t, Loc.t) Ast.Class.Body.element * (Loc.t, Loc.t) Ast.Class.Body.element
  | Type of (Loc.t, Loc.t) Ast.Type.t * (Loc.t, Loc.t) Ast.Type.t
  | Return of (Loc.t, Loc.t) Flow_ast.Function.return

type t = node Utils_js.LocMap.t

module L = Utils_js.LocMap
module B = Flow_ast.Class.Body

class wrapper (m: Flow_ast_mapper.mapper) (s: t ref) =
  object (_this)
    inherit Flow_ast_mapper.mapper as super

    val m = m

    method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
      let stmt = super#statement stmt in
      let (loc_pre, _) = stmt in
      let size_pre = L.cardinal !s in
      let mapped = m#statement stmt in
      if size_pre == L.cardinal !s && mapped <> stmt then (
        s := L.add loc_pre (Statement (stmt, mapped)) !s ;
        mapped )
      else stmt

    method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
      let expr = super#expression expr in
      let (loc_pre, _) = expr in
      let size_pre = L.cardinal !s in
      let mapped = m#expression expr in
      if size_pre == L.cardinal !s && mapped <> expr then (
        s := L.add loc_pre (Expression (expr, mapped)) !s ;
        mapped )
      else expr

    method! class_element (elem: (Loc.t, Loc.t) Ast.Class.Body.element) =
      let elem = super#class_element elem in
      let loc_pre = match elem with
        | B.Method (loc, _) -> loc
        | B.Property (loc, _) -> loc
        | B.PrivateField (loc, _) -> loc
      in
      let size_pre = L.cardinal !s in
      let mapped = m#class_element elem in
      if size_pre == L.cardinal !s && mapped <> elem then (
        s := L.add loc_pre (ClassElement (elem, mapped)) !s ;
        mapped )
      else elem

    method! type_ (t: (Loc.t, Loc.t) Ast.Type.t) =
      let t = super#type_ t in
      let (loc_pre, _) = t in
      let size_pre = L.cardinal !s in
      let mapped = m#type_ t in
      if size_pre == L.cardinal !s && mapped <> t then (
        s := L.add loc_pre (Type (t, mapped)) !s ;
        mapped )
      else t

    method! return_type_annotation (return: (Loc.t, Loc.t) Flow_ast.Function.return) =
      let open Flow_ast.Function in
      let return = super#return_type_annotation return in
      let loc_pre = match return with
      | Available _ -> Loc.none
      | Missing loc -> loc
      in
      let size_pre = L.cardinal !s in
      let mapped = m#return_type_annotation return in
      if size_pre == L.cardinal !s && mapped <> return then (
        s := L.add loc_pre (Return mapped) !s ;
        mapped )
      else return

    method! function_ loc (expr: (Loc.t, Loc.t) Flow_ast.Function.t) =
      let open Flow_ast.Function in
      let { return = return1; _ } = expr in
      let loc_pre = match return1 with
      | Available _ -> Loc.none
      | Missing loc -> loc
      in
      let func' = super#function_ loc expr in
      let { return = return2; _ } = func' in
      let func'' = m#function_ loc func' in
      let { return = return3; _ } = func'' in
      if not (return1 == return2 && return2 == return3) then
        s := L.add loc_pre (Return return3) !s ;
      func''

    method! class_method loc (meth: (Loc.t, Loc.t) Ast.Class.Method.t') =
      let open Flow_ast.Function in
      let ({Ast.Class.Method.value; _} as meth) = super#class_method loc meth in
      let _, { return = return1; _ } = value in
      let loc_pre = match return1 with
      | Available _ -> Loc.none
      | Missing loc -> loc
      in
      let ({Ast.Class.Method.value; _} as meth') = super#class_method loc meth in
      let _, { return = return2; _ } = value in
      let ({Ast.Class.Method.value; _} as meth'') = m#class_method loc meth' in
      let _, { return = return3; _ } = value in
      if not (return1 == return2 && return2 == return3) then
        s := L.add loc_pre (Return return3) !s ;
      meth''
  end

let collapse_diffs map =
  L.fold
    (fun loc elem acc ->
      match
        L.find_first_opt
          (fun loc_candidate ->
            loc <> loc_candidate && Loc.span_compare loc_candidate loc = 0 )
          map
      with
      | Some _ -> acc
      | None -> L.add loc elem acc )
    map L.empty

let diff m ast =
  let s = ref L.empty in
  let w = new wrapper m s in
  let ast = w#program ast in
  let diffs = collapse_diffs !s in
  (diffs, ast)

let combine ~orig ~add =
  if L.is_empty orig then add
  else if L.is_empty add then orig
  else
    (* Adds to orig the elements that intersect between add and orig *)
    let merged_map =
      L.fold
        (fun loc elem acc ->
          match L.find_opt loc add with
          | Some node -> (
            match (elem, node) with
            | Statement (old, _), Statement (_, new_) ->
                L.add loc (Statement (old, new_)) acc
            | Expression (old, _), Expression (_, new_) ->
                L.add loc (Expression (old, new_)) acc
            | _ -> raise Combine_inconsistency )
          | None -> L.add loc elem acc )
        orig L.empty
    in
    (* Adds the elements from add that were not in orig *)
    let merged_map_with_add =
      L.fold
        (fun loc elem acc ->
          match L.find_opt loc acc with
          | Some _ -> acc
          | None -> L.add loc elem acc )
        add merged_map
    in
    collapse_diffs merged_map_with_add
