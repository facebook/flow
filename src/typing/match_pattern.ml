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
open Enclosing_context

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

let object_property_key cx acc ~pattern_kind key :
    (ALoc.t, ALoc.t) Flow_ast.Expression.t
    * (ALoc.t, ALoc.t * Type.t) Ast.MatchPattern.ObjectPattern.Property.key
    * string =
  let open Ast.MatchPattern.ObjectPattern in
  match key with
  | Property.Identifier (loc, { Ast.Identifier.name; comments }) ->
    let acc = object_named_property acc loc name in
    let current = Unsoundness.at Type.NonBindingPattern loc in
    (acc, Property.Identifier ((loc, current), { Ast.Identifier.name; comments }), name)
  | Property.StringLiteral (loc, ({ Ast.StringLiteral.value; _ } as lit)) ->
    let acc = object_named_property acc loc value in
    (acc, Property.StringLiteral (loc, lit), value)
  | Property.NumberLiteral (loc, ({ Ast.NumberLiteral.value; _ } as lit)) ->
    let prop = Dtoa.ecma_string_of_float value in
    if Js_number.is_float_safe_integer value then
      let acc = object_named_property acc loc prop in
      (acc, Property.NumberLiteral (loc, lit), prop)
    else (
      Flow_js.add_output cx (Error_message.EMatchInvalidObjectPropertyLiteral { loc; pattern_kind });
      (acc, Property.NumberLiteral (loc, lit), prop)
    )
  | Property.BigIntLiteral (loc, { Ast.BigIntLiteral.raw; _ }) ->
    Flow_js.add_output cx (Error_message.EMatchInvalidObjectPropertyLiteral { loc; pattern_kind });
    (acc, Tast_utils.error_mapper#match_object_pattern_property_key key, raw)

let binding cx ~on_binding ~kind acc name_loc name =
  let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
  let current = Type_env.find_write cx Env_api.OrdinaryNameLoc reason in
  let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason acc }) in
  on_binding ~use_op ~name_loc ~kind name current

let binding_identifier cx ~on_binding ~in_or_pattern ~kind acc id =
  let (loc, { Ast.Identifier.name; comments }) = id in
  if in_or_pattern then (
    Flow_js.add_output cx (Error_message.EMatchBindingInOrPattern { loc });
    Tast_utils.error_mapper#t_identifier id
  ) else
    let t = binding cx ~on_binding ~kind acc loc name in
    ((loc, t), { Ast.Identifier.name; comments })

let binding_pattern cx ~on_binding ~in_or_pattern ~loc acc binding =
  let open Ast.MatchPattern.BindingPattern in
  let { kind; id; comments } = binding in
  let id =
    match kind with
    | Ast.Variable.Var
    | Ast.Variable.Let ->
      Flow_js.add_output cx (Error_message.EMatchInvalidBindingKind { loc; kind });
      Tast_utils.error_mapper#t_identifier id
    | Ast.Variable.Const -> binding_identifier cx ~on_binding ~in_or_pattern ~kind acc id
  in
  { kind; id; comments }

let rec member cx ~on_identifier ~on_expression mem =
  let open Ast.MatchPattern.MemberPattern in
  let (loc, { base; property; comments }) = mem in
  let (base_exp, base) =
    match base with
    | BaseIdentifier (loc, id) ->
      let exp = (loc, Ast.Expression.Identifier (loc, id)) in
      (exp, BaseIdentifier ((loc, on_identifier ~encl_ctx:OtherTestContext cx id loc), id))
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
    | PropertyBigInt (loc, lit) ->
      let exp = Ast.Expression.Member.PropertyExpression (loc, Ast.Expression.BigIntLiteral lit) in
      (exp, (fun _ -> PropertyBigInt (loc, lit)))
  in
  let exp =
    ( loc,
      Ast.Expression.Member
        { Ast.Expression.Member._object = base_exp; property = property_exp; comments }
    )
  in
  let ((_, t), _) = on_expression cx exp in
  (exp, ((loc, t), { base; property = get_property t; comments }))

let rest_pattern cx ~on_binding ~in_or_pattern acc rest =
  let open Ast.MatchPattern.RestPattern in
  Base.Option.map rest ~f:(fun (rest_loc, { argument; comments }) ->
      ( rest_loc,
        {
          argument =
            Base.Option.map argument ~f:(fun (arg_loc, arg) ->
                (arg_loc, binding_pattern cx ~on_binding ~in_or_pattern ~loc:arg_loc acc arg)
            );
          comments;
        }
      )
  )

let rec pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc (loc, p) :
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
        Base.List.map
          patterns
          ~f:(pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern:true acc)
      in
      OrPattern { OrPattern.patterns; comments }
    | AsPattern { AsPattern.pattern = p; target; comments } ->
      (match p with
      | (_, BindingPattern _) -> Flow_js.add_output cx (Error_message.EMatchInvalidAsPattern { loc })
      | _ -> ());
      let p = pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc p in
      let target =
        match target with
        | AsPattern.Binding (loc, binding) ->
          AsPattern.Binding (loc, binding_pattern cx ~on_binding ~in_or_pattern ~loc acc binding)
        | AsPattern.Identifier id ->
          AsPattern.Identifier
            (binding_identifier cx ~on_binding ~in_or_pattern ~kind:Ast.Variable.Const acc id)
      in
      AsPattern { AsPattern.pattern = p; target; comments }
    | IdentifierPattern (loc, x) ->
      let t = on_identifier ~encl_ctx:OtherTestContext cx x loc in
      IdentifierPattern ((loc, t), x)
    | BindingPattern x -> BindingPattern (binding_pattern cx ~on_binding ~in_or_pattern ~loc acc x)
    | WildcardPattern ({ WildcardPattern.invalid_syntax_default_keyword; _ } as x) ->
      if invalid_syntax_default_keyword then
        Flow_js.add_output cx (Error_message.EMatchInvalidWildcardSyntax loc);
      WildcardPattern x
    | ArrayPattern pattern ->
      ArrayPattern
        (array_pattern cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc pattern)
    | ObjectPattern pattern ->
      ObjectPattern
        (object_pattern
           cx
           ~on_identifier
           ~on_expression
           ~on_binding
           ~in_or_pattern
           ~pattern_kind:Flow_intermediate_error_types.MatchObjPatternKind.Object
           acc
           pattern
        )
    | InstancePattern x when not @@ Context.enable_pattern_matching_instance_patterns cx ->
      Flow_js.add_output
        cx
        (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.MatchInstancePattern));
      InstancePattern (Tast_utils.error_mapper#match_instance_pattern x)
    | InstancePattern
        { InstancePattern.constructor; properties = (properties_loc, properties); comments } ->
      let open InstancePattern in
      let constructor =
        match constructor with
        | IdentifierConstructor (loc, x) ->
          let t = on_identifier ~encl_ctx:OtherTestContext cx x loc in
          IdentifierConstructor ((loc, t), x)
        | MemberConstructor mem ->
          let (_, mem) = member cx ~on_identifier ~on_expression mem in
          MemberConstructor mem
      in
      let properties =
        ( properties_loc,
          object_pattern
            cx
            ~on_identifier
            ~on_expression
            ~on_binding
            ~in_or_pattern
            ~pattern_kind:Flow_intermediate_error_types.MatchObjPatternKind.Instance
            acc
            properties
        )
      in
      InstancePattern { constructor; properties; comments }
  in
  (loc, p)

and array_pattern cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc pattern =
  let open Ast.MatchPattern.ArrayPattern in
  let { elements; rest; comments } = pattern in
  let rest = rest_pattern cx ~on_binding ~in_or_pattern acc rest in
  let elements =
    array_elements cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc elements
  in
  { elements; rest; comments }

and array_elements cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc elements =
  let open Ast.MatchPattern.ArrayPattern in
  Base.List.mapi
    ~f:(fun i { Element.pattern = (loc, p); index } ->
      let acc = array_element acc i loc in
      let p = pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc (loc, p) in
      { Element.pattern = p; index })
    elements

and object_pattern
    cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern ~pattern_kind acc pattern =
  let open Ast.MatchPattern.ObjectPattern in
  let { properties; rest; comments } = pattern in
  let rest = rest_pattern cx ~on_binding ~in_or_pattern acc rest in
  let properties =
    object_properties
      cx
      ~on_identifier
      ~on_expression
      ~on_binding
      ~in_or_pattern
      ~pattern_kind
      acc
      properties
  in
  { properties; rest; comments }

and object_properties
    cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern ~pattern_kind acc props =
  let open Ast.MatchPattern.ObjectPattern in
  let rec loop acc seen rev_props = function
    | [] -> List.rev rev_props
    | (loc, Property.Valid { Property.key; pattern = p; shorthand; comments }) :: props ->
      let (acc, key, name) = object_property_key cx acc ~pattern_kind key in
      ( if SSet.mem name seen then
        let key_loc =
          match key with
          | Property.StringLiteral (loc, _)
          | Property.NumberLiteral (loc, _)
          | Property.BigIntLiteral (loc, _) ->
            loc
          | Property.Identifier ((loc, _), _) ->
            (match p with
            | ( _,
                Ast.MatchPattern.BindingPattern { Ast.MatchPattern.BindingPattern.id = (loc, _); _ }
              )
              when shorthand ->
              loc
            | _ -> loc)
        in
        Flow_js.add_output
          cx
          (Error_message.EMatchDuplicateObjectProperty { loc = key_loc; name; pattern_kind })
      );
      let p = pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern acc p in
      let prop = (loc, Property.Valid { Property.key; pattern = p; shorthand; comments }) in
      loop acc (SSet.add name seen) (prop :: rev_props) props
    | ((loc, Property.InvalidShorthand (_, { Ast.Identifier.name; _ })) as prop) :: props ->
      Flow_js.add_output cx (Error_message.EMatchInvalidObjectShorthand { loc; name; pattern_kind });
      loop acc (SSet.add name seen) (prop :: rev_props) props
  in
  loop acc SSet.empty [] props

let pattern cx ~on_identifier ~on_expression ~on_binding acc (loc, p) =
  pattern_ cx ~on_identifier ~on_expression ~on_binding ~in_or_pattern:false acc (loc, p)

let type_of_member_pattern cx ~on_identifier ~on_expression mem =
  let (_, ((_, t), _)) = member cx ~on_identifier ~on_expression mem in
  t
