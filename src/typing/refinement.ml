(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast

(*
 * type refinements on expressions - wraps Env_js API
 *)

(* if expression is syntactically eligible for type refinement,
   return Some (access key), otherwise None.
   Eligible expressions are simple ids and chains of property|index
   lookups from an id base
 *)
let rec key = Ast.Expression.(function

| _, This ->
  (* treat this as a property chain, in terms of refinement lifetime *)
  Some (Reason_js.internal_name "this", [])

| _, Identifier (_, { Ast.Identifier.name; _ }) when name != "undefined" -> (
  (* ditto super *)
  match name with
  | "super" -> Some (Reason_js.internal_name "super", [])
  | _ -> Some (name, [])
  )

| _, Member { Member._object;
  (* foo.bar.baz -> Chain [Id baz; Id bar; Id foo] *)
   property = (
    Member.PropertyIdentifier (_, { Ast.Identifier.name; _ })
    | Member.PropertyExpression (_, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.String name;
        _;
      })
   ); _; } -> (
  match key _object with
  | Some (base, chain) ->
    Some (base, Key.Prop name :: chain)
  | None -> None
  )

| _, Member {
    Member._object; property = Member.PropertyExpression index; _
  } -> (
  (* foo.bar[baz] -> Chain [Index baz; Id bar; Id foo] *)
  match key _object with
  | Some (base, chain) -> (
    match key index with
    | Some key ->
      Some (base, Key.Elem key :: chain)
    | None -> None
    )
  | None -> None
  )

| _ ->
  (* other LHSes unsupported currently/here *)
  None
)

(* get type refinement for expression, if it exists *)
let get cx expr reason =
  match key expr with
  | Some k -> Env_js.get_refinement cx k reason
  | None -> None
