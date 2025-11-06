(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast

let get_identifier_from_expr expr =
  match expr with
  | (_, Expression.Identifier (_, { Identifier.name; _ })) -> Some name
  | _ -> None

let get_property_name property =
  match property with
  | Expression.Member.PropertyIdentifier (_, { Identifier.name; _ }) -> Some name
  | _ -> None

class property_access_collector base_obj_name =
  object (this)
    inherit [SSet.t, Loc.t] Flow_ast_visitor.visitor ~init:SSet.empty as super

    method! member _loc (expr : (Loc.t, Loc.t) Expression.Member.t) =
      let { Expression.Member._object; property; comments = _ } = expr in
      (match (get_identifier_from_expr _object, get_property_name property) with
      | (Some obj_name, Some prop_name) when obj_name = base_obj_name ->
        this#update_acc (fun acc -> SSet.add prop_name acc)
      | _ -> ());
      super#member _loc expr
  end

let is_simple_literal_pattern pattern =
  match pattern with
  | (_, MatchPattern.StringPattern _)
  | (_, MatchPattern.NumberPattern _)
  | (_, MatchPattern.BigIntPattern _)
  | (_, MatchPattern.BooleanPattern _)
  | (_, MatchPattern.UnaryPattern _)
  | (_, MatchPattern.IdentifierPattern _)
  | (_, MatchPattern.MemberPattern _) ->
    true
  | _ -> false

let create_discriminant_property discriminant_name literal_pattern =
  let (lit_loc, _lit_value) = literal_pattern in
  let key =
    MatchPattern.ObjectPattern.Property.Identifier
      (lit_loc, { Identifier.name = discriminant_name; comments = None })
  in
  let pattern = literal_pattern in
  let property =
    { MatchPattern.ObjectPattern.Property.key; pattern; shorthand = false; comments = None }
  in
  (lit_loc, MatchPattern.ObjectPattern.Property.Valid property)

let create_binding_property prop_name loc =
  let id = (loc, { Identifier.name = prop_name; comments = None }) in
  let binding_pattern =
    { MatchPattern.BindingPattern.kind = Variable.Const; id; comments = None }
  in
  let pattern = (loc, MatchPattern.BindingPattern binding_pattern) in
  let key = MatchPattern.ObjectPattern.Property.Identifier id in
  let property =
    { MatchPattern.ObjectPattern.Property.key; pattern; shorthand = true; comments = None }
  in
  (loc, MatchPattern.ObjectPattern.Property.Valid property)

let create_object_pattern discriminant_name literal_pattern accessed_props loc =
  let discriminant_prop = create_discriminant_property discriminant_name literal_pattern in
  let binding_props =
    Base.List.map ~f:(fun prop -> create_binding_property prop loc) (SSet.elements accessed_props)
  in
  let properties = discriminant_prop :: binding_props in
  {
    MatchPattern.ObjectPattern.properties;
    rest = Some (Loc.none, { MatchPattern.RestPattern.argument = None; comments = None });
    comments = None;
  }

class member_replacer base_obj_name =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! expression expr =
      match expr with
      | ( loc,
          Expression.Member
            {
              Expression.Member._object;
              property = Expression.Member.PropertyIdentifier (_, { Identifier.name; _ });
              _;
            }
        ) ->
        (match get_identifier_from_expr _object with
        | Some obj_name when obj_name = base_obj_name ->
          (loc, Expression.Identifier (loc, { Identifier.name; comments = None }))
        | _ -> super#expression expr)
      | _ -> super#expression expr
  end

class mapper target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    val mutable found : bool = false

    method is_found = found

    method private transform_case_expr base_obj_name discriminant_name case =
      let ( case_loc,
            { Match.Case.pattern; body; guard; comments; invalid_syntax; case_match_root_loc }
          ) =
        case
      in
      if not (is_simple_literal_pattern pattern) then
        case
      else
        let collector = new property_access_collector base_obj_name in
        let _ = collector#expression body in
        let accessed_props = collector#acc in
        let obj_pattern = create_object_pattern discriminant_name pattern accessed_props case_loc in
        let new_pattern = (case_loc, MatchPattern.ObjectPattern obj_pattern) in
        let as_target =
          MatchPattern.AsPattern.Identifier
            (case_loc, { Identifier.name = base_obj_name; comments = None })
        in
        let as_pattern =
          { MatchPattern.AsPattern.pattern = new_pattern; target = as_target; comments = None }
        in
        let final_pattern = (case_loc, MatchPattern.AsPattern as_pattern) in
        let replacer = new member_replacer base_obj_name in
        let new_body = replacer#expression body in
        ( case_loc,
          {
            Match.Case.pattern = final_pattern;
            body = new_body;
            guard;
            comments;
            invalid_syntax;
            case_match_root_loc;
          }
        )

    method private transform_case_stmt base_obj_name discriminant_name case =
      let ( case_loc,
            { Match.Case.pattern; body; guard; comments; invalid_syntax; case_match_root_loc }
          ) =
        case
      in
      if not (is_simple_literal_pattern pattern) then
        case
      else
        let collector = new property_access_collector base_obj_name in
        let _ = collector#statement body in
        let accessed_props = collector#acc in
        let obj_pattern = create_object_pattern discriminant_name pattern accessed_props case_loc in
        let new_pattern = (case_loc, MatchPattern.ObjectPattern obj_pattern) in
        let as_target =
          MatchPattern.AsPattern.Identifier
            (case_loc, { Identifier.name = base_obj_name; comments = None })
        in
        let as_pattern =
          { MatchPattern.AsPattern.pattern = new_pattern; target = as_target; comments = None }
        in
        let final_pattern = (case_loc, MatchPattern.AsPattern as_pattern) in
        let replacer = new member_replacer base_obj_name in
        let new_body = replacer#statement body in
        ( case_loc,
          {
            Match.Case.pattern = final_pattern;
            body = new_body;
            guard;
            comments;
            invalid_syntax;
            case_match_root_loc;
          }
        )

    method private should_transform
        : 'body.
          (Loc.t, Loc.t) Expression.t ->
          (Loc.t, Loc.t, 'body) Match.Case.t list ->
          (string * string) option =
      fun arg cases ->
        match arg with
        | ( _,
            Expression.Member
              {
                Expression.Member._object;
                property = Expression.Member.PropertyIdentifier (_, { Identifier.name; _ });
                _;
              }
          ) ->
          (match get_identifier_from_expr _object with
          | Some base_name ->
            let all_simple =
              List.for_all
                (fun (_, { Match.Case.pattern; _ }) -> is_simple_literal_pattern pattern)
                cases
            in
            if all_simple then
              Some (base_name, name)
            else
              None
          | None -> None)
        | _ -> None

    method! match_expression loc (x : (Loc.t, Loc.t) Expression.match_expression) =
      if found then
        x
      else
        let { Match.arg; cases; match_keyword_loc; comments } = x in
        match this#should_transform arg cases with
        | Some (base_obj_name, discriminant_name) when this#target_contained_by loc ->
          found <- true;
          let new_arg =
            ( fst arg,
              Expression.Identifier
                (fst arg, { Identifier.name = base_obj_name; comments = (snd arg |> fun _ -> None) })
            )
          in
          let new_cases =
            List.map (this#transform_case_expr base_obj_name discriminant_name) cases
          in
          { Match.arg = new_arg; cases = new_cases; match_keyword_loc; comments }
        | _ -> super#match_expression loc x

    method! match_statement loc (x : (Loc.t, Loc.t) Statement.match_statement) =
      if found then
        x
      else
        let { Match.arg; cases; match_keyword_loc; comments } = x in
        match this#should_transform arg cases with
        | Some (base_obj_name, discriminant_name) when this#target_contained_by loc ->
          found <- true;
          let new_arg =
            ( fst arg,
              Expression.Identifier
                (fst arg, { Identifier.name = base_obj_name; comments = (snd arg |> fun _ -> None) })
            )
          in
          let new_cases =
            List.map (this#transform_case_stmt base_obj_name discriminant_name) cases
          in
          { Match.arg = new_arg; cases = new_cases; match_keyword_loc; comments }
        | _ -> super#match_statement loc x
  end

let refactor ast loc =
  let mapper = new mapper loc in
  let ast' = mapper#program ast in
  if (not mapper#is_found) || ast' == ast then
    None
  else
    Some ast'
