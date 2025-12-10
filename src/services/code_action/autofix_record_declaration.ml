(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let rec definitely_includes_void type_ =
  let open Flow_ast.Type in
  match type_ with
  | (_, (Nullable _ | Mixed _ | Unknown _ | Void _ | Any _)) -> true
  | (_, Union { Union.types = (t1, t2, rest); _ })
  | (_, Intersection { Intersection.types = (t1, t2, rest); _ }) ->
    Base.List.exists (t1 :: t2 :: rest) ~f:definitely_includes_void
  | _ -> false

let add_void_union_to_annotation annot =
  let open Flow_ast in
  let (annot_loc, type_) = annot in
  let void_type = (Loc.none, Flow_ast.Type.Void None) in
  let new_type =
    match type_ with
    | type_ when definitely_includes_void type_ -> type_
    | (type_loc, Type.Union { Type.Union.types = (t1, t2, rest); comments = union_comments }) ->
      ( type_loc,
        Type.Union { Type.Union.types = (void_type, t1, t2 :: rest); comments = union_comments }
      )
    | _ ->
      let (type_loc, _) = type_ in
      (type_loc, Type.Union { Type.Union.types = (type_, void_type, []); comments = None })
  in
  (annot_loc, new_type)

class mapper target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method private record_property_internal prop =
      let open Flow_ast.Statement.RecordDeclaration in
      let { Property.key; annot; default_value; comments; invalid_syntax } = prop in
      if Base.Option.is_none invalid_syntax then
        prop
      else
        let (annot, default_value) =
          match invalid_syntax with
          | Some { InvalidPropertySyntax.invalid_optional = Some _; _ } ->
            (* Fix `foo?: number,` as `foo: void | number = undefined,` *)
            let new_annot = add_void_union_to_annotation annot in
            let new_default_value =
              match default_value with
              | None ->
                Some
                  ( Loc.none,
                    Flow_ast.Expression.Identifier
                      (Loc.none, { Flow_ast.Identifier.name = "undefined"; comments = None })
                  )
              | Some _ -> default_value
            in
            (new_annot, new_default_value)
          | _ -> (annot, default_value)
        in
        { Property.key; annot; default_value; comments; invalid_syntax = None }

    method private record_static_property_internal prop =
      let open Flow_ast.Statement.RecordDeclaration in
      let { StaticProperty.key; annot; value; comments; invalid_syntax } = prop in
      if Base.Option.is_none invalid_syntax then
        prop
      else
        let annot =
          match invalid_syntax with
          | Some { InvalidPropertySyntax.invalid_optional = Some _; _ } ->
            add_void_union_to_annotation annot
          | _ -> annot
        in
        { StaticProperty.key; annot; value; comments; invalid_syntax = None }

    method! record_declaration loc decl =
      let open Flow_ast.Statement.RecordDeclaration in
      let { id = (id_loc, _) as id; tparams; implements; body; comments } = decl in
      if this#is_target id_loc then
        let (body_loc, { Body.body = elements; comments = body_comments }) = body in
        let elements =
          Base.List.map elements ~f:(fun element ->
              match element with
              | Body.Property (prop_loc, prop) ->
                let prop' = this#record_property_internal prop in
                Body.Property (prop_loc, prop')
              | Body.StaticProperty (prop_loc, prop) ->
                let prop' = this#record_static_property_internal prop in
                Body.StaticProperty (prop_loc, prop')
              | Body.Method _ -> element
          )
        in
        let body = (body_loc, { Body.body = elements; comments = body_comments }) in
        { id; tparams; implements; body; comments }
      else
        super#record_declaration loc decl

    method! record_property loc prop =
      let { Flow_ast.Statement.RecordDeclaration.Property.invalid_syntax; _ } = prop in
      if this#target_contained_by loc && Base.Option.is_some invalid_syntax then
        this#record_property_internal prop
      else
        super#record_property loc prop

    method! record_static_property loc prop =
      let { Flow_ast.Statement.RecordDeclaration.StaticProperty.invalid_syntax; _ } = prop in
      if this#target_contained_by loc && Base.Option.is_some invalid_syntax then
        this#record_static_property_internal prop
      else
        super#record_static_property loc prop
  end

let fix_invalid_syntax ast loc =
  let mapper = new mapper loc in
  mapper#program ast
