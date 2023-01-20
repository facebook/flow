(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target_loc kind =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private is_target loc = Loc.equal target_loc loc

    method loc_annot_contains_target loc = Loc.contains loc target_loc

    method! type_ t =
      let open Flow_ast.Type in
      match t with
      | (loc, Unknown comments) when kind = `UnknownType && this#is_target loc ->
        Ast_builder.Types.mixed ?comments ()
      | (loc, Never comments) when kind = `NeverType && this#is_target loc ->
        Ast_builder.Types.empty ?comments ()
      | (loc, Undefined comments) when kind = `UndefinedType && this#is_target loc ->
        Ast_builder.Types.void ?comments ()
      | (loc, Keyof { Keyof.argument; comments }) when kind = `KeyofType && this#is_target loc ->
        let targs = Ast_builder.Types.type_args [super#type_ argument] in
        Ast_builder.Types.unqualified_generic ?comments ~targs "$Keys"
      | (loc, ReadOnly { ReadOnly.argument = (_, Array { Array.argument; _ }); comments })
        when kind = `ReadOnlyArrayType && this#is_target loc ->
        let targs = Ast_builder.Types.type_args [super#type_ argument] in
        Ast_builder.Types.unqualified_generic ?comments ~targs "$ReadOnlyArray"
      | (loc, ReadOnly { ReadOnly.argument = (_, Tuple _) as argument; comments })
        when kind = `ReadOnlyTupleType && this#is_target loc ->
        let targs = Ast_builder.Types.type_args [super#type_ argument] in
        Ast_builder.Types.unqualified_generic ?comments ~targs "$ReadOnly"
      | _ -> super#type_ t

    method! type_param (loc, tparam) =
      let open Flow_ast.Type.TypeParam in
      match tparam with
      | { bound_kind = Extends; _ } when kind = `TypeParamExtends && this#is_target loc ->
        (Loc.none, { tparam with bound_kind = Colon })
      | { variance = Some (v_loc, Flow_ast.Variance.{ kind = InOut; _ }); _ }
        when kind = `InOutVariance && this#is_target v_loc ->
        (Loc.none, { tparam with variance = None })
      | _ -> super#type_param (loc, tparam)

    method! variance variance =
      let open Flow_ast.Variance in
      let (loc, { kind = variance_kind; comments }) = variance in
      match variance_kind with
      | Readonly when kind = `ReadonlyVariance && this#is_target loc ->
        (Loc.none, { kind = Plus; comments })
      | In when kind = `InVariance && this#is_target loc -> (Loc.none, { kind = Minus; comments })
      | Out when kind = `OutVariance && this#is_target loc -> (Loc.none, { kind = Plus; comments })
      | _ -> super#variance variance

    method! expression e =
      let open Flow_ast.Expression in
      match e with
      | (loc, TSTypeCast { TSTypeCast.expression; kind = TSTypeCast.As annot; comments })
        when kind = `AsExpression && this#is_target loc ->
        let expression = super#expression expression in
        let annot = super#type_ annot in
        Ast_builder.Expressions.typecast ?comments expression annot
      | (loc, TSTypeCast { TSTypeCast.expression; kind = TSTypeCast.Satisfies annot; comments })
        when kind = `SatisfiesExpression && this#is_target loc ->
        let expression = super#expression expression in
        let annot = super#type_ annot in
        Ast_builder.Expressions.typecast ?comments expression annot
      | _ -> super#expression e
  end

let convert_unknown_type ast loc =
  let mapper = new mapper loc `UnknownType in
  mapper#program ast

let convert_never_type ast loc =
  let mapper = new mapper loc `NeverType in
  mapper#program ast

let convert_undefined_type ast loc =
  let mapper = new mapper loc `UndefinedType in
  mapper#program ast

let convert_keyof_type ast loc =
  let mapper = new mapper loc `KeyofType in
  mapper#program ast

let convert_type_param_extends ast loc =
  let mapper = new mapper loc `TypeParamExtends in
  mapper#program ast

let convert_readonly_variance ast loc =
  let mapper = new mapper loc `ReadonlyVariance in
  mapper#program ast

let convert_in_variance ast loc =
  let mapper = new mapper loc `InVariance in
  mapper#program ast

let convert_out_variance ast loc =
  let mapper = new mapper loc `OutVariance in
  mapper#program ast

let remove_in_out_variance ast loc =
  let mapper = new mapper loc `InOutVariance in
  mapper#program ast

let convert_as_expression ast loc =
  let mapper = new mapper loc `AsExpression in
  mapper#program ast

let convert_satisfies_expression ast loc =
  let mapper = new mapper loc `SatisfiesExpression in
  mapper#program ast

let convert_readonly_array_type ast loc =
  let mapper = new mapper loc `ReadOnlyArrayType in
  mapper#program ast

let convert_readonly_tuple_type ast loc =
  let mapper = new mapper loc `ReadOnlyTupleType in
  mapper#program ast
