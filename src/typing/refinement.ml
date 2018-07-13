(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


(*
 * type refinements on expressions - wraps Env API
 *)

(* if expression is syntactically eligible for type refinement,
   return Some (access key), otherwise None.
   Eligible expressions are simple ids and chains of property|index
   lookups from an id base
 *)
let rec key = Ast.Expression.(function

| _, This ->
  (* treat this as a property chain, in terms of refinement lifetime *)
  Some (Reason.internal_name "this", [])

| _, Super ->
  (* treat this as a property chain, in terms of refinement lifetime *)
  Some (Reason.internal_name "super", [])

| _, Identifier (_, name) when name != "undefined" ->
  Some (name, [])

| _, Member { Member._object;
  (* foo.bar.baz -> Chain [Id baz; Id bar; Id foo] *)
   property = (
    Member.PropertyIdentifier (_, name)
    | Member.PropertyExpression (_, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.String name;
        _;
      })
    | Member.PropertyExpression (_, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.Number _;
        raw = name;
      })
   ); _; } -> (
  match key _object with
  | Some (base, chain) ->
    Some (base, Key.Prop name :: chain)
  | None -> None
  )

| _, Member {
    Member._object; property = Member.PropertyPrivateName (_, (_, name)); _
  } -> (
  match key _object with
  | Some (base, chain) ->
    Some (base, Key.PrivateField name :: chain)
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
let get cx expr loc =
  match key expr with
  | Some k -> Env.get_refinement cx k loc
  | None -> None
