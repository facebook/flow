(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap
module Scopes = Scope_api.With_ALoc

let rec import_stars stmts =
  let open Ast.Statement in
  match stmts with
  | [] -> ALocMap.empty
  | ( _,
      ImportDeclaration
        {
          ImportDeclaration.specifiers =
            Some (ImportDeclaration.ImportNamespaceSpecifier ((_, (id_loc, _)) as specifier));
          _;
        } )
    :: rest ->
    ALocMap.add id_loc specifier (import_stars rest)
  | _ :: rest -> import_stars rest

class import_export_visitor ~cx ~scope_info ~import_stars =
  object (this)
    inherit [unit, ALoc.t] Flow_ast_visitor.visitor ~init:() as super

    method private add_error err = Flow_js.add_output cx err

    method private add_bad_default_import_access_error loc import_star =
      let (import_star_loc, _) = import_star in
      let import_star_reason = Reason.mk_reason (Reason.RCode "import *") import_star_loc in
      this#add_error (Error_message.EBadDefaultImportAccess (loc, import_star_reason))

    method private import_star_from_use use =
      let { Scopes.Def.locs; _ } = Scopes.def_of_use scope_info use in
      Nel.fold_left
        (fun acc def_loc ->
          match acc with
          | Some _ -> acc
          | None -> ALocMap.find_opt def_loc import_stars)
        None
        locs

    method! member expr_loc expr =
      let open Ast.Expression in
      match expr with
      | {
       Member._object = (_, Identifier (id_loc, _));
       property =
         ( Member.PropertyIdentifier (_, { Ast.Identifier.name = "default"; _ })
         | Member.PropertyExpression
             (_, Literal { Ast.Literal.value = Ast.Literal.String "default"; _ }) );
       _;
      } ->
        (match this#import_star_from_use id_loc with
        | Some import_star ->
          this#add_bad_default_import_access_error expr_loc import_star;
          expr
        | None -> super#member expr_loc expr)
      | _ -> super#member expr_loc expr

    method object_pattern_default_property object_pattern =
      let open Ast.Pattern.Object in
      let { properties; _ } = object_pattern in
      Base.List.find_map
        ~f:(fun prop ->
          let open Property in
          match prop with
          | Ast.Pattern.Object.Property
              ( _,
                {
                  key =
                    ( Identifier (default_loc, { Ast.Identifier.name = "default"; _ })
                    | Literal (default_loc, { Ast.Literal.value = Ast.Literal.String "default"; _ })
                      );
                  _;
                } ) ->
            Some default_loc
          | _ -> None)
        properties

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      match decl with
      | ( _,
          {
            id = (_, Ast.Pattern.Object object_pattern);
            init = Some (_, Ast.Expression.Identifier (id_loc, _));
          } ) ->
        let default_loc = this#object_pattern_default_property object_pattern in
        let import_star = this#import_star_from_use id_loc in
        (match (default_loc, import_star) with
        | (Some default_loc, Some import_star) ->
          this#add_bad_default_import_access_error default_loc import_star;
          decl
        | _ -> super#variable_declarator ~kind decl)
      | _ -> super#variable_declarator ~kind decl

    method! assignment loc assign =
      let open Ast.Expression.Assignment in
      match assign with
      | {
       left = (_, Ast.Pattern.Object object_pattern);
       right = (_, Ast.Expression.Identifier (id_loc, _));
       _;
      } ->
        let default_loc = this#object_pattern_default_property object_pattern in
        let import_star = this#import_star_from_use id_loc in
        (match (default_loc, import_star) with
        | (Some default_loc, Some import_star) ->
          this#add_bad_default_import_access_error default_loc import_star;
          assign
        | _ -> super#assignment loc assign)
      | _ -> super#assignment loc assign
  end

let detect_errors cx ast =
  let (_, { Ast.Program.statements; _ }) = ast in
  let scope_info = Scope_builder.With_ALoc.program ast in
  let import_stars = import_stars statements in
  let visitor = new import_export_visitor ~cx ~scope_info ~import_stars in
  ignore (visitor#program ast)
