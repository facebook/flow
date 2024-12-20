(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Tast_utils = Typed_ast_utils
open Reason
open Type

let array_element acc i loc =
  ( loc,
    let open Ast.Expression in
    Member
      {
        Member._object = acc;
        property =
          Member.PropertyExpression
            ( loc,
              Ast.Expression.NumberLiteral
                { Ast.NumberLiteral.value = float i; raw = string_of_int i; comments = None }
            );
        comments = None;
      }
  )

let object_named_property acc loc name =
  ( loc,
    let open Ast.Expression in
    Member
      {
        Member._object = acc;
        property = Member.PropertyIdentifier (loc, { Ast.Identifier.name; comments = None });
        comments = None;
      }
  )

let object_property_key cx acc key :
    (ALoc.t, ALoc.t) Flow_ast.Expression.t
    * (ALoc.t, ALoc.t * Type.t) Ast.MatchPattern.ObjectPattern.Property.key =
  let open Ast.MatchPattern.ObjectPattern in
  match key with
  | Property.Identifier (loc, { Ast.Identifier.name; comments }) ->
    let acc = object_named_property acc loc name in
    let current = Unsoundness.at Type.NonBindingPattern loc in
    (acc, Property.Identifier ((loc, current), { Ast.Identifier.name; comments }))
  | Property.StringLiteral (loc, ({ Ast.StringLiteral.value; _ } as lit)) ->
    let acc = object_named_property acc loc value in
    (acc, Property.StringLiteral (loc, lit))
  | Property.NumberLiteral (loc, ({ Ast.NumberLiteral.value; _ } as lit)) ->
    if Js_number.is_float_safe_integer value then
      let prop = Dtoa.ecma_string_of_float value in
      let acc = object_named_property acc loc prop in
      (acc, Property.NumberLiteral (loc, lit))
    else (
      Flow_js.add_output cx (Error_message.EMatchInvalidObjectPropertyLiteral { loc });
      (acc, Property.NumberLiteral (loc, lit))
    )

let binding cx ~on_binding ~kind acc name_loc name =
  let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
  let current = Type_env.find_write cx Env_api.OrdinaryNameLoc reason in
  let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason acc }) in
  on_binding ~use_op ~name_loc ~kind name current

let binding_identifier cx ~on_binding ~kind acc (loc, { Ast.Identifier.name; comments }) =
  let t = binding cx ~on_binding ~kind acc loc name in
  ((loc, t), { Ast.Identifier.name; comments })

let binding_pattern cx ~on_binding ~loc acc binding =
  let open Ast.MatchPattern.BindingPattern in
  let { kind; id; comments } = binding in
  let id =
    match kind with
    | Ast.Variable.Var
    | Ast.Variable.Let ->
      Flow_js.add_output cx (Error_message.EMatchInvalidBindingKind { loc; kind });
      Tast_utils.error_mapper#t_identifier id
    | Ast.Variable.Const -> binding_identifier cx ~on_binding ~kind acc id
  in
  { kind; id; comments }

let rec member cx ~on_identifier ~on_expression mem =
  let open Ast.MatchPattern.MemberPattern in
  let (loc, { base; property; comments }) = mem in
  let (base_exp, base) =
    match base with
    | BaseIdentifier (loc, id) ->
      let exp = (loc, Ast.Expression.Identifier (loc, id)) in
      (exp, BaseIdentifier ((loc, on_identifier cx id loc), id))
    | BaseMember mem ->
      let (exp, mem) = member cx ~on_identifier ~on_expression mem in
      (exp, BaseMember mem)
  in
  let (property_exp, get_property) =
    match property with
    | PropertyIdentifier (loc, id) ->
      let exp = Ast.Expression.Member.PropertyIdentifier (loc, id) in
      (exp, (fun t -> PropertyIdentifier ((loc, t), id)))
    | PropertyString (loc, lit) ->
      let exp = Ast.Expression.Member.PropertyExpression (loc, Ast.Expression.StringLiteral lit) in
      (exp, (fun _ -> PropertyString (loc, lit)))
    | PropertyNumber (loc, lit) ->
      let exp = Ast.Expression.Member.PropertyExpression (loc, Ast.Expression.NumberLiteral lit) in
      (exp, (fun _ -> PropertyNumber (loc, lit)))
  in
  let exp =
    ( loc,
      Ast.Expression.Member
        { Ast.Expression.Member._object = base_exp; property = property_exp; comments }
    )
  in
  let ((_, t), _) = on_expression cx exp in
  (exp, ((loc, t), { base; property = get_property t; comments }))

let rest_pattern cx ~on_binding acc rest =
  let open Ast.MatchPattern.RestPattern in
  Base.Option.map rest ~f:(fun (rest_loc, { argument; comments }) ->
      ( rest_loc,
        {
          argument =
            Base.Option.map argument ~f:(fun (arg_loc, arg) ->
                (arg_loc, binding_pattern cx ~on_binding ~loc:arg_loc acc arg)
            );
          comments;
        }
      )
  )

let rec pattern cx ~on_identifier ~on_expression ~on_binding acc (loc, p) :
    (ALoc.t, ALoc.t * Type.t) Ast.MatchPattern.t =
  let open Ast.MatchPattern in
  let p =
    match p with
    | NumberPattern x -> NumberPattern x
    | BigIntPattern x -> BigIntPattern x
    | StringPattern x -> StringPattern x
    | BooleanPattern x -> BooleanPattern x
    | NullPattern x -> NullPattern x
    | UnaryPattern x ->
      let { UnaryPattern.operator; argument; _ } = x in
      (match (operator, argument) with
      | (_, (_, UnaryPattern.NumberLiteral { Ast.NumberLiteral.value = 0.0; _ })) ->
        Flow_js.add_output cx (Error_message.EMatchInvalidUnaryZero { loc })
      | (UnaryPattern.Plus, (_, UnaryPattern.BigIntLiteral _)) ->
        Flow_js.add_output cx (Error_message.EMatchInvalidUnaryPlusBigInt { loc })
      | _ -> ());
      UnaryPattern x
    | MemberPattern mem ->
      let (_, mem) = member cx ~on_identifier ~on_expression mem in
      MemberPattern mem
    | OrPattern { OrPattern.patterns; comments } ->
      let patterns =
        Base.List.map patterns ~f:(pattern cx ~on_identifier ~on_expression ~on_binding acc)
      in
      OrPattern { OrPattern.patterns; comments }
    | AsPattern { AsPattern.pattern = p; target; comments } ->
      let p = pattern cx ~on_identifier ~on_expression ~on_binding acc p in
      let target =
        match target with
        | AsPattern.Binding (loc, binding) ->
          AsPattern.Binding (loc, binding_pattern cx ~on_binding ~loc acc binding)
        | AsPattern.Identifier id ->
          AsPattern.Identifier (binding_identifier cx ~on_binding ~kind:Ast.Variable.Const acc id)
      in
      AsPattern { AsPattern.pattern = p; target; comments }
    | IdentifierPattern (loc, x) ->
      let t = on_identifier cx x loc in
      IdentifierPattern ((loc, t), x)
    | BindingPattern x -> BindingPattern (binding_pattern cx ~on_binding ~loc acc x)
    | WildcardPattern x -> WildcardPattern x
    | ArrayPattern { ArrayPattern.elements; rest; comments } ->
      let rest = rest_pattern cx ~on_binding acc rest in
      let elements = array_elements cx ~on_identifier ~on_expression ~on_binding acc elements in
      ArrayPattern { ArrayPattern.elements; rest; comments }
    | ObjectPattern { ObjectPattern.properties; rest; comments } ->
      let rest = rest_pattern cx ~on_binding acc rest in
      let properties =
        object_properties cx ~on_identifier ~on_expression ~on_binding acc properties
      in
      ObjectPattern { ObjectPattern.properties; rest; comments }
  in
  (loc, p)

and array_elements cx ~on_identifier ~on_expression ~on_binding acc elements =
  let open Ast.MatchPattern.ArrayPattern in
  Base.List.mapi
    ~f:(fun i { Element.pattern = (loc, p); index } ->
      let acc = array_element acc i loc in
      let p = pattern cx ~on_identifier ~on_expression ~on_binding acc (loc, p) in
      { Element.pattern = p; index })
    elements

and object_properties cx ~on_identifier ~on_expression ~on_binding acc props =
  let open Ast.MatchPattern.ObjectPattern in
  let rec loop acc rev_props = function
    | [] -> List.rev rev_props
    | (loc, { Property.key; pattern = p; shorthand; comments }) :: props ->
      let (acc, key) = object_property_key cx acc key in
      let p = pattern cx ~on_identifier ~on_expression ~on_binding acc p in
      let prop = (loc, { Property.key; pattern = p; shorthand; comments }) in
      loop acc (prop :: rev_props) props
  in
  loop acc [] props
