(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target fixed_prop_name =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, Member ({ Member.property; _ } as m)) ->
        (match property with
        | Member.PropertyIdentifier (property_id_loc, { Flow_ast.Identifier.comments; _ })
          when this#is_target property_id_loc ->
          ( loc,
            Member
              {
                m with
                Member.property =
                  Member.PropertyIdentifier
                    (property_id_loc, { Flow_ast.Identifier.name = fixed_prop_name; comments });
              }
          )
        | Member.PropertyExpression
            ( property_literal_loc,
              Literal { Flow_ast.Literal.value = Flow_ast.Literal.String _; comments; _ }
            )
          when this#is_target property_literal_loc ->
          let fixed_prop_name_literal = Ast_builder.Literals.string ~comments fixed_prop_name in
          ( loc,
            Member
              {
                m with
                Member.property =
                  Member.PropertyExpression (property_literal_loc, Literal fixed_prop_name_literal);
              }
          )
        | _ -> super#expression e)
      | _ -> super#expression e
  end

let replace_prop_typo_at_target ~fixed_prop_name ast loc =
  let mapper = new mapper loc fixed_prop_name in
  mapper#program ast
