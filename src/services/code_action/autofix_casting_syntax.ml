(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc =
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
        when this#is_target loc ->
        this#build_cast ?comments expression annot
      | _ -> super#expression e
  end

let convert_colon_cast ast loc =
  let mapper = new mapper loc in
  mapper#program ast

class all_mapper =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method private build_cast ?comments expression annot =
      let expression = super#expression expression in
      let annot = super#type_ annot in
      Ast_builder.Expressions.as_expression ?comments expression annot

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (_, TypeCast { TypeCast.expression; annot = (_, annot); comments }) ->
        this#build_cast ?comments expression annot
      | _ -> super#expression e
  end

let convert_all_colon_casts ast =
  let mapper = new all_mapper in
  mapper#program ast
