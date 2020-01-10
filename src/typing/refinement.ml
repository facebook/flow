(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(*
 * type refinements on expressions - wraps Env API
 *)

(* if expression is syntactically eligible for type refinement,
   return Some (access key), otherwise None.
   Eligible expressions are simple ids and chains of property|index
   lookups from an id base
 *)
let rec key ~allow_optional =
  let open Ast.Expression in
  function
  | (_, This) ->
    (* treat this as a property chain, in terms of refinement lifetime *)
    Some (Reason.internal_name "this", [])
  | (_, Super) ->
    (* treat this as a property chain, in terms of refinement lifetime *)
    Some (Reason.internal_name "super", [])
  | (_, Identifier id) -> key_of_identifier id
  | (_, OptionalMember { OptionalMember.member; _ }) when allow_optional ->
    key_of_member ~allow_optional member
  | (_, Member member) -> key_of_member ~allow_optional member
  | _ ->
    (* other LHSes unsupported currently/here *)
    None

and key_of_identifier (_, { Ast.Identifier.name; comments = _ }) =
  if name = "undefined" then
    None
  else
    Some (name, [])

and key_of_member ~allow_optional { Ast.Expression.Member._object; property; _ } =
  let open Ast.Expression.Member in
  match property with
  | PropertyIdentifier (_, { Ast.Identifier.name; comments = _ })
  | PropertyExpression (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String name; _ })
  | PropertyExpression
      ( _,
        Ast.Expression.Literal
          { Ast.Literal.value = Ast.Literal.Number _; raw = name; comments = _ } ) ->
    (match key ~allow_optional _object with
    | Some (base, chain) -> Some (base, Key.Prop name :: chain)
    | None -> None)
  | PropertyPrivateName (_, (_, { Ast.Identifier.name; comments = _ })) ->
    (match key ~allow_optional _object with
    | Some (base, chain) -> Some (base, Key.PrivateField name :: chain)
    | None -> None)
  | PropertyExpression index ->
    (* foo.bar[baz] -> Chain [Index baz; Id bar; Id foo] *)
    (match key ~allow_optional _object with
    | Some (base, chain) ->
      (match key ~allow_optional index with
      | Some key -> Some (base, Key.Elem key :: chain)
      | None -> None)
    | None -> None)

let key_of_pattern ~allow_optional patt =
  match patt with
  | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) -> key_of_identifier name
  | (_, Ast.Pattern.Expression (_, Ast.Expression.Member member)) ->
    key_of_member ~allow_optional member
  | ( _,
      Ast.Pattern.Expression
        (_, Ast.Expression.OptionalMember { Ast.Expression.OptionalMember.member; _ }) ) ->
    key_of_member ~allow_optional member
  | (_, Ast.Pattern.Array _)
  | (_, Ast.Pattern.Object _) ->
    (* other LHSes unsupported currently/here *)
    None
  | (_, Ast.Pattern.Expression _) ->
    (* non-member expression patterns are bogus *)
    None

(* get type refinement for expression, if it exists *)
let get ~allow_optional cx expr loc =
  match key ~allow_optional expr with
  | Some k -> Env.get_refinement cx k loc
  | None -> None

let get_of_pattern ~allow_optional cx patt loc =
  match key_of_pattern ~allow_optional patt with
  | Some k -> Env.get_refinement cx k loc
  | None -> None
