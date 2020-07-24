(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

exception Found of string

let find documentation = raise (Found documentation)

let find_description comments =
  Base.Option.iter (Jsdoc.of_comments comments) ~f:(Jsdoc.description %> Base.Option.iter ~f:find)

let loc_of_object_key =
  let open Flow_ast.Expression.Object.Property in
  function
  | Identifier (loc, _)
  | Literal (loc, _)
  | Computed (_, Flow_ast.ComputedKey.{ expression = (loc, _); _ })
  | PrivateName (loc, _) ->
    loc

let comments_of_variance =
  let open Flow_ast.Variance in
  Base.Option.bind ~f:(fun (_, { comments; _ }) -> comments)

let comments_of_object_key =
  let open Flow_ast.Expression.Object.Property in
  function
  | Identifier (_, Flow_ast.Identifier.{ comments; _ })
  | Literal (_, Flow_ast.Literal.{ comments; _ })
  | Computed (_, Flow_ast.ComputedKey.{ comments; _ }) ->
    comments
  | PrivateName _ -> None

let loc_of_annotation_or_hint =
  let open Flow_ast.Type in
  function
  | Missing loc
  | Available (_, (loc, _)) ->
    loc

class documentation_searcher (def_loc : Loc.t) =
  object (this)
    inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

    method is_target loc = Loc.equal def_loc loc

    method! variable_declaration stmt_loc decl =
      let open Flow_ast.Statement.VariableDeclaration in
      let { declarations; comments; _ } = decl in
      Base.List.iter declarations ~f:(function
          | ( _,
              Declarator.
                { id = (_, Flow_ast.Pattern.(Identifier Identifier.{ name = (loc, _); _ })); _ } )
            when this#is_target loc ->
            find_description comments
          | (_, Declarator.{ init = Some (loc, _); _ }) when this#is_target loc ->
            find_description comments
          | _ -> ());
      super#variable_declaration stmt_loc decl

    method! class_ stmt_loc cls =
      let open Flow_ast.Class in
      let { id; comments; _ } = cls in
      Base.Option.iter id ~f:(fun (loc, _) -> if this#is_target loc then find_description comments);
      super#class_ stmt_loc cls

    method! function_ loc func =
      let open Flow_ast.Function in
      let { comments; id; _ } = func in
      if this#is_target loc then find_description comments;
      Base.Option.iter id ~f:(fun (id_loc, _) ->
          if this#is_target id_loc then find_description comments);
      super#function_ loc func

    method! declare_variable stmt_loc decl =
      let open Flow_ast.Statement.DeclareVariable in
      let { id = (loc, _); comments; _ } = decl in
      if this#is_target loc then find_description comments;
      super#declare_variable stmt_loc decl

    method! declare_class stmt_loc decl =
      let open Flow_ast.Statement.DeclareClass in
      let { id = (loc, _); comments; _ } = decl in
      if this#is_target loc then find_description comments;
      super#declare_class stmt_loc decl

    method! declare_function stmt_loc decl =
      let open Flow_ast.Statement.DeclareFunction in
      let { id = (loc, _); comments; _ } = decl in
      if this#is_target loc then find_description comments;
      super#declare_function stmt_loc decl

    method! object_property_type prop_type =
      let open Flow_ast.Type.Object.Property in
      let (_, { key; value; comments; variance; _ }) = prop_type in
      let value_loc =
        match value with
        | Init (value_loc, _)
        | Get (_, Flow_ast.Type.Function.{ return = (value_loc, _); _ })
        | Set (value_loc, _) ->
          value_loc
      in
      if this#is_target (loc_of_object_key key) || this#is_target value_loc then begin
        find_description comments;
        find_description (comments_of_variance variance);
        find_description (comments_of_object_key key)
      end;
      super#object_property_type prop_type

    method! class_method method_loc meth =
      let open Flow_ast.Class.Method in
      let { key; comments; _ } = meth in
      if this#is_target (loc_of_object_key key) then begin
        find_description comments;
        find_description (comments_of_object_key key)
      end;
      super#class_method method_loc meth

    method! class_property prop_loc prop =
      let open Flow_ast.Class.Property in
      let { key; variance; comments; _ } = prop in
      if this#is_target (loc_of_object_key key) then begin
        find_description comments;
        find_description (comments_of_variance variance);
        find_description (comments_of_object_key key)
      end;
      super#class_property prop_loc prop

    method! object_property prop =
      let open Flow_ast.Expression.Object.Property in
      let (locs, comments) =
        match prop with
        | (_, Init { key; value = (value_loc, _); _ }) ->
          ([loc_of_object_key key; value_loc], [comments_of_object_key key])
        | ( _,
            Method { key; value = (_, Flow_ast.Function.{ params = (value_loc, _); comments; _ }) }
          ) ->
          ([loc_of_object_key key; value_loc], [comments_of_object_key key; comments])
        | (_, Get { key; value = (_, Flow_ast.Function.{ return; _ }); comments }) ->
          ([loc_of_object_key key; loc_of_annotation_or_hint return], [comments])
        | (_, Set _) -> ([], [])
      in
      if List.exists this#is_target locs then List.iter find_description comments;
      super#object_property prop
  end

let documentation_of_def_loc def_loc ast =
  let searcher = new documentation_searcher def_loc in
  try
    ignore (searcher#program ast);
    None
  with Found documentation -> Some documentation

let of_getdef_loc ~reader def_loc =
  let open Base.Option in
  Loc.source def_loc >>= Parsing_heaps.Reader.get_ast ~reader >>= documentation_of_def_loc def_loc
