(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class property_access_searcher name =
  object (this)
    inherit [bool, Loc.t] Flow_ast_visitor.visitor ~init:false as super

    method! member loc expr =
      Ast.Expression.Member.(
        begin
          match expr.property with
          | PropertyIdentifier (_, { Ast.Identifier.name = id; comments = _ }) when id = name ->
            this#set_acc true
          | _ -> ()
        end;
        super#member loc expr)

    method! object_key (key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
      Ast.Expression.Object.Property.(
        begin
          match key with
          | Identifier (_, { Ast.Identifier.name = id; comments = _ }) when id = name ->
            this#set_acc true
          | _ -> ()
        end;
        super#object_key key)

    method! pattern_object_property ?kind (prop : (Loc.t, Loc.t) Ast.Pattern.Object.Property.t') =
      Ast.Pattern.Object.Property.(
        let { key; _ } = prop in
        begin
          match key with
          | Identifier (_, { Ast.Identifier.name = id; comments = _ }) when id = name ->
            this#set_acc true
          | _ -> ()
        end;
        super#pattern_object_property ?kind prop)

    method! export_default_declaration
        loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t) =
      if name = "default" then this#set_acc true;
      super#export_default_declaration loc decl

    method! export_named_declaration
        loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportNamedDeclaration.t) =
      Ast.Statement.ExportNamedDeclaration.(
        let { declaration; _ } = decl in
        Ast.Statement.(
          begin
            match declaration with
            | Some
                ( _,
                  FunctionDeclaration
                    {
                      Ast.Function.id =
                        Some (_, { Ast.Identifier.name = exported_name; comments = _ });
                      _;
                    } )
            | Some
                ( _,
                  ClassDeclaration
                    {
                      Ast.Class.id = Some (_, { Ast.Identifier.name = exported_name; comments = _ });
                      _;
                    } ) ->
              if exported_name = name then this#set_acc true
            | Some (_, VariableDeclaration { VariableDeclaration.declarations = decls; _ }) ->
              Flow_ast_utils.fold_bindings_of_variable_declarations
                (fun () (_, { Ast.Identifier.name = exported_name; comments = _ }) ->
                  if exported_name = name then this#set_acc true)
                ()
                decls
            | _ -> () (* TODO add type exports when find-refs supports them *)
          end;

          (* TODO specifiers *)
          super#export_named_declaration loc decl))

    method! import_declaration loc (decl : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t) =
      Ast.Statement.ImportDeclaration.(
        let { default; specifiers; _ } = decl in
        if Option.is_some default && name = "default" then this#set_acc true;
        let handle_specifier = function
          (* `import * as ...`
           * No action needed since any references to actual exports will appear later as normal
           * property references. *)
          | ImportNamespaceSpecifier _ -> ()
          | ImportNamedSpecifiers named_specifiers ->
            named_specifiers
            |> List.iter
                 (fun { remote = (_, { Ast.Identifier.name = remote_name; comments = _ }); _ } ->
                   if remote_name = name then this#set_acc true)
        in
        Option.iter specifiers ~f:handle_specifier;
        super#import_declaration loc decl)
  end

(* Returns true iff the given AST contains an access to a property with the given name *)
let search name ast =
  let checker = new property_access_searcher name in
  checker#eval checker#program ast
