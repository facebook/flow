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
      | _ -> super#type_ t
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
