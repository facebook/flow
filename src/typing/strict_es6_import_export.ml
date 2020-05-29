(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocSet = Loc_collections.ALocSet
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

    method private import_star_reason import_star =
      let (import_star_loc, _) = import_star in
      Reason.mk_reason (Reason.RCode "import *") import_star_loc

    method private add_bad_default_import_access_error loc import_star =
      let import_star_reason = this#import_star_reason import_star in
      this#add_error (Error_message.EBadDefaultImportAccess (loc, import_star_reason))

    method private add_bad_default_import_destructuring_error loc =
      this#add_error (Error_message.EBadDefaultImportDestructuring loc)

    method private add_invalid_import_star_use_error loc import_star =
      let import_star_reason = this#import_star_reason import_star in
      this#add_error (Error_message.EInvalidImportStarUse (loc, import_star_reason))

    method private import_star_from_use =
      (* Create a map from import star use locs to the import star specifier *)
      let import_star_uses =
        ALocMap.fold
          (fun def_loc specifier all_uses ->
            let def = Scopes.def_of_use scope_info def_loc in
            let uses = Scopes.uses_of_def scope_info def in
            ALocSet.fold
              (fun use_loc all_uses -> ALocMap.add use_loc specifier all_uses)
              uses
              all_uses)
          import_stars
          ALocMap.empty
      in
      (fun use -> ALocMap.find_opt use import_star_uses)

    method private is_import_star_use use = this#import_star_from_use use <> None

    method! expression expr =
      let open Ast.Expression in
      match expr with
      (* Error on use of module object. Valid use of module object will not recurse to this point. *)
      | (_, Identifier (id_loc, _)) ->
        (match this#import_star_from_use id_loc with
        | Some import_star ->
          this#add_invalid_import_star_use_error id_loc import_star;
          expr
        | None -> super#expression expr)
      | _ -> super#expression expr

    method! member expr_loc expr =
      let open Ast.Expression in
      match expr with
      | { Member._object = (_, Identifier (id_loc, _)); property; _ } ->
        let import_star = this#import_star_from_use id_loc in
        (match (import_star, property) with
        (* Error on attempt to access default export *)
        | (Some import_star, Member.PropertyIdentifier (_, { Ast.Identifier.name = "default"; _ }))
        | ( Some import_star,
            Member.PropertyExpression
              (_, Literal { Ast.Literal.value = Ast.Literal.String "default"; _ }) ) ->
          this#add_bad_default_import_access_error expr_loc import_star;
          expr
        (* Do not recurse on valid use of module object *)
        | (Some _, Member.PropertyIdentifier _)
        | ( Some _,
            Member.PropertyExpression (_, Literal { Ast.Literal.value = Ast.Literal.String _; _ })
          ) ->
          expr
        | _ -> super#member expr_loc expr)
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
            id = (_, Ast.Pattern.Object object_pattern) as id;
            init = Some (_, Ast.Expression.Identifier (id_loc, _));
          } ) ->
        (match this#import_star_from_use id_loc with
        | Some import_star ->
          (* Error on attempt to access default export *)
          begin
            match this#object_pattern_default_property object_pattern with
            | Some default_loc -> this#add_bad_default_import_access_error default_loc import_star
            | None -> ()
          end;
          (* Do not recurse into init since init is valid use of module object *)
          ignore (this#variable_declarator_pattern ~kind id);
          decl
        | None -> super#variable_declarator ~kind decl)
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
        (* Error on attempt to access default export *)
        | (Some default_loc, Some import_star) ->
          this#add_bad_default_import_access_error default_loc import_star;
          assign
        (* Do not recurse since RHS is a valid use of module object *)
        | (None, Some _) -> assign
        | _ -> super#assignment loc assign)
      | _ -> super#assignment loc assign

    method! import_declaration loc decl =
      let open Ast.Statement.ImportDeclaration in
      begin
        match decl with
        | { specifiers = Some (ImportNamedSpecifiers specifiers); _ } ->
          List.iter
            (fun specifier ->
              match specifier with
              | { remote = (default_loc, { Ast.Identifier.name = "default"; _ }); _ } ->
                this#add_bad_default_import_destructuring_error default_loc
              | _ -> ())
            specifiers
        | _ -> ()
      end;
      super#import_declaration loc decl

    method! generic_identifier_type git =
      let open Ast.Type.Generic.Identifier in
      match git with
      (* Error on unqualified use of module object *)
      | Unqualified (id_loc, _) ->
        (match this#import_star_from_use id_loc with
        | Some import_star ->
          this#add_invalid_import_star_use_error id_loc import_star;
          git
        | None -> super#generic_identifier_type git)
      (* Do not recurse on valid use of module object *)
      | Qualified (_, { qualification = Unqualified (id_loc, _); _ })
        when this#is_import_star_use id_loc ->
        git
      | _ -> super#generic_identifier_type git

    method! jsx_element elem_loc elem =
      let open Ast.JSX in
      let { openingElement = (_, { Opening.name; _ }); _ } = elem in
      begin
        match name with
        (* Error on use of module object outside member expression *)
        | Identifier (id_loc, _) when this#is_import_star_use id_loc ->
          (match this#import_star_from_use id_loc with
          | Some import_star -> this#add_invalid_import_star_use_error id_loc import_star
          | None -> ())
        | _ -> ()
      end;
      super#jsx_element elem_loc elem
  end

let detect_errors cx ast =
  let (_, { Ast.Program.statements; _ }) = ast in
  let scope_info = Scope_builder.With_ALoc.program ast in
  let import_stars = import_stars statements in
  let visitor = new import_export_visitor ~cx ~scope_info ~import_stars in
  ignore (visitor#program ast)
