(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type proj =
  | Prop of string
  | Elem of t
  | PrivateField of string
[@@deriving show]

and t = {
  base: string;
  projections: proj list;
}
[@@deriving show]

(* These functions are all either slightly modified or directly copied from src/typing/refinement.ml.
 * Eventually this module will replace that one. *)

let rec debug_string_of_key { base; projections } =
  base
  ^ String.concat
      ""
      (List.rev projections
      |> Base.List.map ~f:(function
             | Prop name -> spf ".%s" name
             | PrivateField name -> spf "private.%s" name
             | Elem expr -> spf "[%s]" (debug_string_of_key expr)))

(* true if the given key uses the given property name *)
let rec uses_propname propname ~private_ { projections; base = _ } =
  proj_uses_propname ~private_ propname projections

(* true if the given projection list uses the given property name *)
and proj_uses_propname ~private_ propname = function
  | Prop name :: tail ->
    (name = propname && not private_) || proj_uses_propname ~private_ propname tail
  | PrivateField name :: tail ->
    (name = propname && private_) || proj_uses_propname ~private_ propname tail
  | Elem key :: tail ->
    uses_propname ~private_ propname key || proj_uses_propname ~private_ propname tail
  | [] -> false

let compare = Stdlib.compare

let reason_desc =
  Reason.(
    function
    | ((OrdinaryName _ as name), []) -> RIdentifier name
    | (name, []) -> RCustom (display_string_of_name name)
    | (_, projs) ->
      (match List.hd (List.rev projs) with
      | Prop x -> RProperty (Some (OrdinaryName x))
      | PrivateField x -> RPrivateProperty x
      | Elem _ -> RProperty None))

(* These functions are adapted from typing/refinement.ml. Eventually, this will be the only place
 * where refinement logic lives, so jmbrown is ok with this temporary duplication while he is
 * fleshing out the refinement features of EnvBuilder
 *
 * The purpose of these functions is to extract _what_ is being refined when we have something like
 * expr != null. What in expr does this refine? *)
let rec key ?(allow_optional = true) =
  let open Flow_ast.Expression in
  function
  | (_, Identifier id) -> Some (key_of_identifier id)
  | (_, OptionalMember { OptionalMember.member; _ }) when allow_optional ->
    key_of_member ~allow_optional member
  | (_, Member member) -> key_of_member ~allow_optional member
  | _ ->
    (* other LHSes unsupported currently/here *)
    None

and key_of_member ~allow_optional { Flow_ast.Expression.Member._object; property; _ } =
  let open Flow_ast.Expression.Member in
  match property with
  | PropertyIdentifier (_, { Flow_ast.Identifier.name; comments = _ })
  | PropertyExpression
      (_, Flow_ast.Expression.Literal { Flow_ast.Literal.value = Flow_ast.Literal.String name; _ })
  | PropertyExpression
      ( _,
        Flow_ast.Expression.Literal
          { Flow_ast.Literal.value = Flow_ast.Literal.Number _; raw = name; comments = _ } ) ->
    (match key ~allow_optional _object with
    | Some { base; projections } -> Some { base; projections = Prop name :: projections }
    | None -> None)
  | PropertyPrivateName (_, { Flow_ast.PrivateName.name; comments = _ }) ->
    (match key ~allow_optional _object with
    | Some { base; projections } -> Some { base; projections = PrivateField name :: projections }
    | None -> None)
  | PropertyExpression index ->
    (* foo.bar[baz] -> Chain [Index baz; Id bar; Id foo] *)
    (match key ~allow_optional _object with
    | Some { base; projections } ->
      (match key ~allow_optional index with
      | Some key -> Some { base; projections = Elem key :: projections }
      | None -> None)
    | None -> None)

and key_of_identifier (_, { Flow_ast.Identifier.name; comments = _ }) =
  { base = name; projections = [] }

and key_of_argument =
  let open Flow_ast.Expression in
  function
  | Spread _ -> None
  | Expression e -> key e

let property_of_sentinel_refinement { Flow_ast.Expression.Member.property; _ } =
  let open Flow_ast in
  match property with
  | Expression.Member.PropertyIdentifier (_, { Identifier.name = prop_name; _ })
  | Expression.Member.PropertyExpression
      (_, Expression.Literal { Literal.value = Literal.String prop_name; _ }) ->
    Some prop_name
  | _ -> None

let key_of_name name = { base = name; projections = [] }

let key_of_name_with_projections base projections = { base; projections }

let rec key_of_optional_chain expr =
  let open Flow_ast.Expression in
  match expr with
  | (_, Call _) -> None
  | (_, Member _) -> None
  | ( _,
      OptionalMember
        {
          OptionalMember.member =
            { Member._object = (_, Identifier (_, { Flow_ast.Identifier.name; _ })); _ };
          _;
        } ) ->
    Some (key_of_name name)
  | (_, OptionalMember { OptionalMember.member = { Member._object = subject; _ }; _ })
  | (_, OptionalCall { OptionalCall.call = { Call.callee = subject; _ }; _ }) ->
    key_of_optional_chain subject
  | _ -> None

let key_of_pattern patt =
  match patt with
  | (_, Flow_ast.Pattern.Identifier { Flow_ast.Pattern.Identifier.name; _ }) ->
    Some (key_of_identifier name)
  | _ ->
    (* TODO: Heap refinements *)
    None

let empty_projection : proj list = []
