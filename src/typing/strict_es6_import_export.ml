(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap
module ALocSet = Loc_collections.ALocSet
module Scopes = Scope_api.With_ALoc

type var_decl_info = {
  decl_loc: ALoc.t;
  name: string;
  kind: Ast.Statement.VariableDeclaration.kind;
}

type declarations = {
  (* Locs of ids and their import specifier for all `import *` declarations *)
  import_stars: (ALoc.t * (ALoc.t, ALoc.t) Ast.Identifier.t) ALocMap.t;
  (* Locs of ids and information about the declaration for all variable declarations *)
  var_decls: var_decl_info ALocMap.t;
  (* Locs of ids and the function they are bound to *)
  functions: (ALoc.t, ALoc.t) Ast.Function.t ALocMap.t;
  first_import: ALoc.t option;
  first_require: ALoc.t option;
}

let declarations_init =
  {
    import_stars = ALocMap.empty;
    var_decls = ALocMap.empty;
    functions = ALocMap.empty;
    first_import = None;
    first_require = None;
  }

let rec is_require =
  let open Ast.Expression in
  let open Ast.Literal in
  function
  | (_, Member { Member._object; _ }) -> is_require _object
  | ( _,
      Call
        {
          Call.callee = (_, Identifier (_, { Ast.Identifier.name = "require"; _ }));
          arguments =
            (_, { ArgList.arguments = [Expression (_, Literal { value = String _; _ })]; _ });
          _;
        } ) ->
    true
  | _ -> false

let has_value_import =
  let open Ast.Statement.ImportDeclaration in
  function
  | { importKind = ImportValue; specifiers = Some (ImportNamedSpecifiers specifiers); _ } ->
    List.exists
      (fun { Ast.Statement.ImportDeclaration.kind; _ } -> kind = None || kind = Some ImportValue)
      specifiers
  | { importKind = ImportValue; _ } -> true
  | _ -> false

(* Gather information about top level declarations to be used when checking for import/export errors. *)
let gather_declarations ast =
  let open Ast.Statement in
  let add_import_star acc specifier =
    let (_, (id_loc, _)) = specifier in
    { acc with import_stars = ALocMap.add id_loc specifier acc.import_stars }
  in
  let add_var_decl acc id_loc decl_loc name kind =
    { acc with var_decls = ALocMap.add id_loc { decl_loc; name; kind } acc.var_decls }
  in
  let add_function acc id_loc func =
    { acc with functions = ALocMap.add id_loc func acc.functions }
  in
  let add_import acc import_loc =
    match acc with
    | { first_import = None; _ } -> { acc with first_import = Some import_loc }
    | _ -> acc
  in
  let add_require acc require_loc =
    match acc with
    | { first_require = None; _ } -> { acc with first_require = Some require_loc }
    | _ -> acc
  in
  let (_, { Ast.Program.statements; _ }) = ast in
  List.fold_left
    (fun acc stmt ->
      match stmt with
      | (loc, VariableDeclaration { VariableDeclaration.kind; declarations; _ }) ->
        List.fold_left
          (fun acc (_, { VariableDeclaration.Declarator.id; init }) ->
            (* Gather all identifiers in variable declaration *)
            let acc =
              Flow_ast_utils.fold_bindings_of_pattern
                (fun acc (id_loc, { Ast.Identifier.name; _ }) ->
                  add_var_decl acc id_loc loc name kind)
                acc
                id
            in
            (* Gather simple variable declarations where the init is a function, of the forms:
             const <ID> = function() { ... }
             const <ID> = () => { ... } *)
            let acc =
              match (id, init) with
              | ( (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (id_loc, _); _ }),
                  Some (_, (Ast.Expression.ArrowFunction func | Ast.Expression.Function func)) ) ->
                add_function acc id_loc func
              | _ -> acc
            in
            (* Gather require loc if this is a require statement *)
            match init with
            | Some init when is_require init -> add_require acc loc
            | _ -> acc)
          acc
          declarations
      | (loc, ImportDeclaration import) ->
        (* Gather import loc if this import statement imports a value *)
        let acc =
          if has_value_import import then
            add_import acc loc
          else
            acc
        in
        (match import with
        | {
         ImportDeclaration.specifiers = Some (ImportDeclaration.ImportNamespaceSpecifier specifier);
         _;
        } ->
          add_import_star acc specifier
        | _ -> acc)
      | (_, FunctionDeclaration ({ Ast.Function.id = Some (id_loc, _); _ } as func)) ->
        add_function acc id_loc func
      | _ -> acc)
    declarations_init
    statements

(* Visitor that finds locs for each usage of `this` outside a class. *)
class this_visitor =
  object (this)
    inherit [ALoc.t list, ALoc.t] Flow_ast_visitor.visitor ~init:[]

    method! this_expression loc expr =
      this#update_acc (fun locs -> loc :: locs);
      expr

    (* `this` is allowed in classes so do not recurse into class body *)
    method! class_body body = body
  end

(* Visitor that uses the previously found declaration info to check for errors in imports/exports. *)
class import_export_visitor ~cx ~scope_info ~declarations =
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

    method private add_non_const_var_export_error loc decl_info =
      let decl_reason =
        Base.Option.map
          ~f:(fun (decl_loc, name) -> Reason.mk_reason (Reason.RIdentifier name) decl_loc)
          decl_info
      in
      this#add_error (Error_message.ENonConstVarExport (loc, decl_reason))

    method private add_this_in_exported_function_error loc =
      this#add_error (Error_message.EThisInExportedFunction loc)

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
          declarations.import_stars
          ALocMap.empty
      in
      (fun use -> ALocMap.find_opt use import_star_uses)

    method private is_import_star_use use = this#import_star_from_use use <> None

    method add_exported_this_errors func =
      let open Ast.Function in
      let this_visitor = new this_visitor in
      let this_locs = this_visitor#eval this_visitor#function_body_any func.body in
      List.iter this#add_this_in_exported_function_error this_locs

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

    method! export_named_declaration loc decl =
      let open Ast.Statement in
      let open ExportNamedDeclaration in
      let { declaration; specifiers; _ } = decl in
      (* Only const variables can be exported *)
      begin
        match declaration with
        | Some
            ( loc,
              VariableDeclaration
                { VariableDeclaration.kind = VariableDeclaration.Var | VariableDeclaration.Let; _ }
            ) ->
          this#add_non_const_var_export_error loc None
        | _ -> ()
      end;
      (* Check for usage of this in exported functions *)
      begin
        match declaration with
        | Some (_, FunctionDeclaration func) -> this#add_exported_this_errors func
        | Some (_, VariableDeclaration { VariableDeclaration.declarations; _ }) ->
          List.iter
            (fun (_, { VariableDeclaration.Declarator.init; _ }) ->
              match init with
              | Some (_, (Ast.Expression.ArrowFunction func | Ast.Expression.Function func)) ->
                this#add_exported_this_errors func
              | _ -> ())
            declarations
        | _ -> ()
      end;
      begin
        match specifiers with
        | Some (ExportSpecifiers specifiers) ->
          List.iter
            (fun (_, { ExportSpecifier.local = (id_loc, _); _ }) ->
              let def = Scopes.def_of_use scope_info id_loc in
              let def_loc = Nel.hd def.Scopes.Def.locs in
              (* Also check for non-const variables in list of export specifiers *)
              begin
                match ALocMap.find_opt def_loc declarations.var_decls with
                | Some { decl_loc; name; kind = VariableDeclaration.Var | VariableDeclaration.Let }
                  ->
                  this#add_non_const_var_export_error id_loc (Some (decl_loc, name))
                | _ -> ()
              end;
              (* Check for `this` if exported variable is bound to a function *)
              match ALocMap.find_opt def_loc declarations.functions with
              | Some func -> this#add_exported_this_errors func
              | _ -> ())
            specifiers
        | _ -> ()
      end;
      super#export_named_declaration loc decl

    method! export_default_declaration loc decl =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { declaration; _ } = decl in
      begin
        match declaration with
        | Declaration (_, Ast.Statement.FunctionDeclaration func)
        | Expression (_, Ast.Expression.ArrowFunction func)
        | Expression (_, Ast.Expression.Function func) ->
          this#add_exported_this_errors func
        | _ -> ()
      end;
      super#export_default_declaration loc decl
  end

let detect_mixed_import_and_require_error cx declarations =
  match declarations with
  | { first_import = Some first_import_loc; first_require = Some first_require_loc; _ } ->
    let import_reason = Reason.mk_reason (Reason.RCode "import") first_import_loc in
    Flow_js.add_output cx (Error_message.EMixedImportAndRequire (first_require_loc, import_reason))
  | _ -> ()

let detect_errors cx ast =
  let scope_info = Scope_builder.With_ALoc.program ast in
  let declarations = gather_declarations ast in
  detect_mixed_import_and_require_error cx declarations;
  let visitor = new import_export_visitor ~cx ~scope_info ~declarations in
  ignore (visitor#program ast)
