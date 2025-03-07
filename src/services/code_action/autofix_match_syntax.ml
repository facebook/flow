(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | ObjShorthandToConst
  | ObjShorthandToReference

class mapper target_loc ~kind =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! match_object_pattern_property prop =
      let open Flow_ast.MatchPattern.ObjectPattern in
      let (loc, _) = prop in
      if not @@ this#is_target loc then
        super#match_object_pattern_property prop
      else
        match prop with
        | (_, Property.Valid _) -> super#match_object_pattern_property prop
        | (_, Property.InvalidShorthand id) ->
          let key = Property.Identifier id in
          (match kind with
          | ObjShorthandToConst ->
            let pattern =
              ( Loc.none,
                Flow_ast.MatchPattern.BindingPattern
                  {
                    Flow_ast.MatchPattern.BindingPattern.kind = Flow_ast.Variable.Const;
                    id;
                    comments = None;
                  }
              )
            in
            (Loc.none, Property.Valid { Property.key; pattern; shorthand = true; comments = None })
          | ObjShorthandToReference ->
            let pattern = (Loc.none, Flow_ast.MatchPattern.IdentifierPattern id) in
            (Loc.none, Property.Valid { Property.key; pattern; shorthand = false; comments = None }))
  end

let convert_object_shorthand_to_const ast loc =
  let mapper = new mapper loc ~kind:ObjShorthandToConst in
  mapper#program ast

let convert_object_shorthand_to_reference ast loc =
  let mapper = new mapper loc ~kind:ObjShorthandToReference in
  mapper#program ast
