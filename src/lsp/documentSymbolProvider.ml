(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let name_of_key (key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) : string option =
  let open Ast.Expression.Object.Property in
  match key with
  | Literal (_, { Ast.Literal.raw; _ }) -> Some raw
  | Identifier (_, { Ast.Identifier.name = id; comments = _ }) -> Some id
  | PrivateName
      (_, { Ast.PrivateName.id = (_, { Ast.Identifier.name = id; comments = _ }); comments = _ }) ->
    Some id
  | Computed (_, _) -> None

let name_of_id ((_, { Ast.Identifier.name; comments = _ }) : (Loc.t, Loc.t) Ast.Identifier.t) :
    string =
  name

let name_of_id_opt (id_opt : (Loc.t, Loc.t) Ast.Identifier.t option) : string option =
  Base.Option.map id_opt ~f:name_of_id

let ast_name
    ~(uri : Lsp.DocumentUri.t)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(loc : Loc.t)
    ~(containerName : string option)
    ~(name : string)
    ~(kind : Lsp.SymbolInformation.symbolKind) : Lsp.SymbolInformation.t list =
  if name = "" then
    (* sometimes due to parse errors, we end up with empty names. hide them!
       in fact, VS Code throws out the entire response if any symbol name is falsy!
       https://github.com/microsoft/vscode/blob/afd102cbd2e17305a510701d7fd963ec2528e4ea/src/vs/workbench/api/common/extHostTypes.ts#L1068-L1072 *)
    acc
  else
    {
      Lsp.SymbolInformation.name;
      kind;
      location = { Lsp.Location.uri; range = Flow_lsp_conversions.loc_to_lsp_range loc };
      containerName;
    }
    :: acc

let ast_name_opt ~uri ~containerName ~acc ~loc ~(name_opt : string option) ~kind =
  Base.Option.value_map name_opt ~default:acc ~f:(fun name ->
      ast_name ~uri ~containerName ~acc ~loc ~name ~kind)

let ast_key
    ~uri ~containerName ~acc ~loc ~(key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) ~kind =
  ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_key key) ~kind

let ast_id ~uri ~containerName ~acc ~loc ~(id : (Loc.t, Loc.t) Ast.Identifier.t) ~kind =
  ast_name ~uri ~containerName ~acc ~loc ~name:(name_of_id id) ~kind

let ast_id_opt
    ~uri ~containerName ~acc ~loc ~(id_opt : (Loc.t, Loc.t) Ast.Identifier.t option) ~kind =
  ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_id_opt id_opt) ~kind

let ast_class_member
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    (acc : Lsp.SymbolInformation.t list)
    (member : (Loc.t, Loc.t) Ast.Class.Body.element) : Lsp.SymbolInformation.t list =
  let open Ast.Class in
  match member with
  | Body.Method (loc, { Method.kind = Method.Constructor; key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Constructor
  | Body.Method (loc, { Method.kind = Method.Method; key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Method
  | Body.Method (loc, { Method.kind = Method.Get; key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Property
  | Body.Method (loc, { Method.kind = Method.Set; key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Property
  | Body.Property (loc, { Property.key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Property
  | Body.PrivateField
      ( loc,
        {
          PrivateField.key =
            (_, { Ast.PrivateName.id = (_, { Ast.Identifier.name; comments = _ }); comments = _ });
          _;
        } ) ->
    ast_name ~uri ~containerName ~acc ~loc ~name ~kind:Lsp.SymbolInformation.Field

let ast_class
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(loc : Loc.t)
    ~(class_ : (Loc.t, Loc.t) Ast.Class.t) : Lsp.SymbolInformation.t list =
  let open Ast.Class in
  let acc =
    ast_id_opt ~uri ~containerName ~acc ~loc ~id_opt:class_.id ~kind:Lsp.SymbolInformation.Class
  in
  let containerName = name_of_id_opt class_.id in
  let (_, body) = class_.body in
  Base.List.fold body.Body.body ~init:acc ~f:(ast_class_member ~uri ~containerName)

let ast_type_object_property
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    (acc : Lsp.SymbolInformation.t list)
    (property : (Loc.t, Loc.t) Ast.Type.Object.property) : Lsp.SymbolInformation.t list =
  let open Ast.Type.Object in
  match property with
  | Property (loc, { Property.key; _ }) ->
    ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Property
  | Indexer (loc, { Indexer.id; _ }) ->
    ast_id_opt ~uri ~containerName ~acc ~loc ~id_opt:id ~kind:Lsp.SymbolInformation.Property
  | InternalSlot (loc, { InternalSlot.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Property
  | _ -> acc

let ast_type_object
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(object_ : (Loc.t, Loc.t) Ast.Type.Object.t) : Lsp.SymbolInformation.t list =
  let open Ast.Type.Object in
  Base.List.fold object_.properties ~init:acc ~f:(ast_type_object_property ~uri ~containerName)

let ast_type
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(type_ : (Loc.t, Loc.t) Ast.Type.t') : Lsp.SymbolInformation.t list =
  let open Ast.Type in
  match type_ with
  | Object object_ -> ast_type_object ~uri ~containerName ~acc ~object_
  | Interface { Interface.body = (_, object_); _ } ->
    ast_type_object ~uri ~containerName ~acc ~object_
  | _ -> acc

let ast_statement_declaration
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(declaration : (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.declaration) :
    Lsp.SymbolInformation.t list =
  let open Ast.Statement.DeclareExportDeclaration in
  let open Ast.Statement in
  match declaration with
  | Variable (loc, { DeclareVariable.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Variable
  | Function (loc, { DeclareFunction.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Function
  | Class (loc, { DeclareClass.id; body = (_, object_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
    ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
  | DefaultType (_, type_) -> ast_type ~uri ~containerName ~acc ~type_
  | NamedType (loc, { TypeAlias.id; right = (_, type_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
    ast_type ~uri ~containerName:(Some (name_of_id id)) ~acc ~type_
  | NamedOpaqueType (loc, { OpaqueType.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
  | Interface (loc, { Interface.id; body = (_, object_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Interface in
    ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_

let ast_expression
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    ~(acc : Lsp.SymbolInformation.t list)
    ~(expression : (Loc.t, Loc.t) Ast.Expression.t) : Lsp.SymbolInformation.t list =
  let open Ast.Expression in
  match expression with
  | (loc, Class class_) -> ast_class ~uri ~containerName ~acc ~loc ~class_
  | (_, _) -> acc

let rec ast_statement
    ~(uri : Lsp.DocumentUri.t)
    ~(containerName : string option)
    (acc : Lsp.SymbolInformation.t list)
    (statement : (Loc.t, Loc.t) Ast.Statement.t) : Lsp.SymbolInformation.t list =
  let open Ast.Statement in
  match statement with
  | (_, Expression { Expression.expression; _ }) ->
    ast_expression ~uri ~containerName ~acc ~expression
  | (loc, FunctionDeclaration { Ast.Function.id; _ }) ->
    ast_id_opt ~uri ~containerName ~acc ~loc ~id_opt:id ~kind:Lsp.SymbolInformation.Function
  | (loc, ClassDeclaration class_) -> ast_class ~uri ~containerName ~acc ~loc ~class_
  | (loc, InterfaceDeclaration { Interface.id; body = (_, object_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Interface in
    ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
  | (_, ExportNamedDeclaration { ExportNamedDeclaration.declaration = Some stmt; _ }) ->
    ast_statement ~uri ~containerName acc stmt
  | ( _,
      ExportDefaultDeclaration
        { ExportDefaultDeclaration.declaration = ExportDefaultDeclaration.Declaration stmt; _ } ) ->
    ast_statement ~uri ~containerName acc stmt
  | (loc, TypeAlias { TypeAlias.id; right = (_, type_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
    ast_type ~uri ~containerName:(Some (name_of_id id)) ~acc ~type_
  | (loc, OpaqueType { OpaqueType.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
  | (_, VariableDeclaration { VariableDeclaration.declarations; kind; comments = _ }) ->
    let kind =
      match kind with
      | VariableDeclaration.Var -> Lsp.SymbolInformation.Variable
      | VariableDeclaration.Let -> Lsp.SymbolInformation.Variable
      | VariableDeclaration.Const -> Lsp.SymbolInformation.Constant
    in
    let ast_pattern acc loc (_, pattern) =
      let open Ast.Pattern in
      match pattern with
      | Identifier { Identifier.name; _ } -> ast_id ~uri ~containerName ~acc ~loc ~id:name ~kind
      | _ -> acc
    in
    let ast_declarator acc (loc, declarator) =
      ast_pattern acc loc declarator.VariableDeclaration.Declarator.id
    in
    Base.List.fold declarations ~init:acc ~f:ast_declarator
  | (loc, DeclareClass { DeclareClass.id; body = (_, object_); _ }) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
    ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
  | (loc, DeclareFunction { DeclareFunction.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Function
  | ( loc,
      DeclareModule
        { DeclareModule.id = DeclareModule.Identifier id; body = (_, { Block.body; _ }); _ } ) ->
    let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Module in
    let containerName = Some (name_of_id id) in
    Base.List.fold body ~init:acc ~f:(ast_statement ~uri ~containerName)
  | (loc, DeclareVariable { DeclareVariable.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Variable
  | (loc, DeclareOpaqueType { OpaqueType.id; _ }) ->
    ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
  | (_, DeclareExportDeclaration { DeclareExportDeclaration.declaration = Some declaration; _ }) ->
    ast_statement_declaration ~uri ~containerName ~acc ~declaration
  | _ -> acc

let provide_symbol_information ~uri program =
  let (_loc, { Ast.Program.statements; _ }) = program in
  statements |> Base.List.fold ~init:[] ~f:(ast_statement ~uri ~containerName:None) |> Base.List.rev
