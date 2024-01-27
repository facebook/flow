(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper ~enabled_casting_syntax target_loc kind =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method private build_cast ?comments expression annot =
      let expression = super#expression expression in
      let annot = super#type_ annot in
      let open Options.CastingSyntax in
      match enabled_casting_syntax with
      | As
      | Both ->
        Ast_builder.Expressions.as_expression ?comments expression annot
      | Colon -> Ast_builder.Expressions.typecast ?comments expression annot

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, TypeCast { TypeCast.expression; annot = (_, annot); comments })
        when kind = `ColonCast && this#is_target loc ->
        this#build_cast ?comments expression annot
      | (loc, AsExpression { AsExpression.expression; annot = (_, annot); comments })
        when kind = `AsExpression && this#is_target loc ->
        this#build_cast ?comments expression annot
      | (loc, TSSatisfies { TSSatisfies.expression; annot = (_, annot); comments })
        when kind = `SatisfiesExpression && this#is_target loc ->
        this#build_cast ?comments expression annot
      | _ -> super#expression e
  end

let convert_as_expression ~enabled_casting_syntax ast loc =
  let mapper = new mapper ~enabled_casting_syntax loc `AsExpression in
  mapper#program ast

let convert_satisfies_expression ~enabled_casting_syntax ast loc =
  let mapper = new mapper ~enabled_casting_syntax loc `SatisfiesExpression in
  mapper#program ast

let convert_colon_cast ~enabled_casting_syntax ast loc =
  let mapper = new mapper ~enabled_casting_syntax loc `ColonCast in
  mapper#program ast
