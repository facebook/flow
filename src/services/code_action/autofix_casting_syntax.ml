(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc kind =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method private build_cast ?comments expression annot =
      let expression = super#expression expression in
      let annot = super#type_ annot in
      Ast_builder.Expressions.as_expression ?comments expression annot

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, TypeCast { TypeCast.expression; annot = (_, annot); comments })
        when kind = `ColonCast && this#is_target loc ->
        this#build_cast ?comments expression annot
      | (loc, TSSatisfies { TSSatisfies.expression; annot = (_, annot); comments })
        when kind = `SatisfiesExpression && this#is_target loc ->
        this#build_cast ?comments expression annot
      | _ -> super#expression e
  end

let convert_satisfies_expression ast loc =
  let mapper = new mapper loc `SatisfiesExpression in
  mapper#program ast

let convert_colon_cast ast loc =
  let mapper = new mapper loc `ColonCast in
  mapper#program ast

class all_mapper kind =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method private build_cast ?comments expression annot =
      let expression = super#expression expression in
      let annot = super#type_ annot in
      Ast_builder.Expressions.as_expression ?comments expression annot

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (_, TypeCast { TypeCast.expression; annot = (_, annot); comments }) when kind = `ColonCast
        ->
        this#build_cast ?comments expression annot
      | (_, TSSatisfies { TSSatisfies.expression; annot = (_, annot); comments })
        when kind = `SatisfiesExpression ->
        this#build_cast ?comments expression annot
      | _ -> super#expression e
  end

let convert_all_colon_casts ast =
  let mapper = new all_mapper `ColonCast in
  mapper#program ast

let convert_all_satisfies_expressions ast =
  let mapper = new all_mapper `SatisfiesExpression in
  mapper#program ast
