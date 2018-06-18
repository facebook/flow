(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class property_access_searcher name = object(this)
  inherit [bool] Flow_ast_visitor.visitor ~init:false as super
  method! member expr =
    let open Ast.Expression.Member in
    begin match expr.property with
      | PropertyIdentifier (_, x) when x = name ->
          this#set_acc true
      | _ -> ()
    end;
    super#member expr
  method! object_key (key: Loc.t Ast.Expression.Object.Property.key) =
    let open Ast.Expression.Object.Property in
    begin match key with
    | Identifier (_, x) when x = name ->
      this#set_acc true
    | _ -> ()
    end;
    super#object_key key
  method! pattern_object_property ?kind (prop: Loc.t Ast.Pattern.Object.Property.t') =
    let open Ast.Pattern.Object.Property in
    let { key; _ } = prop in
    begin match key with
    | Identifier (_, x) when x = name ->
      this#set_acc true
    | _ -> ()
    end;
    super#pattern_object_property ?kind prop
  method! export_default_declaration loc (decl: Loc.t Ast.Statement.ExportDefaultDeclaration.t) =
    if name = "default" then begin
      this#set_acc true
    end;
    super#export_default_declaration loc decl
  method! export_named_declaration loc (decl: Loc.t Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { declaration; _ } = decl in
    let open Ast.Statement in
    begin match declaration with
    | Some (_, FunctionDeclaration { Ast.Function.id = Some (_, exported_name); _ })
    | Some (_, ClassDeclaration { Ast.Class.id = Some (_, exported_name); _ }) ->
      if exported_name = name then
        this#set_acc true
    | Some (_, VariableDeclaration { VariableDeclaration.declarations = decls; _ }) ->
      Ast_utils.bindings_of_variable_declarations decls
      |> List.iter (fun (_, exported_name) -> if exported_name = name then this#set_acc true)
    | _ -> ()
    (* TODO add type exports when find-refs supports them *)
    end;
    (* TODO specifiers *)
    super#export_named_declaration loc decl
  method! import_declaration loc (decl: Loc.t Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { default; specifiers; _ } = decl in
    if Option.is_some default && name = "default" then begin
      this#set_acc true
    end;
    let handle_specifier = function
      (* `import * as ...`
       * No action needed since any references to actual exports will appear later as normal
       * property references. *)
      | ImportNamespaceSpecifier _ -> ()
      | ImportNamedSpecifiers named_specifiers ->
        named_specifiers |> List.iter begin fun {remote=(_, remote_name); _} ->
          if remote_name = name then
            this#set_acc true
        end
    in
    Option.iter specifiers ~f:handle_specifier;
    super#import_declaration loc decl
end

(* Returns true iff the given AST contains an access to a property with the given name *)
let search name ast =
  let checker = new property_access_searcher name in
  checker#eval checker#program ast
