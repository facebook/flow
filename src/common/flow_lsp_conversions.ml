(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let flow_completion_to_lsp
    (item: ServerProt.Response.complete_autocomplete_result)
  : Lsp.Completion.completionItem =
  let open Lsp.Completion in
  let open ServerProt.Response in
  let trunc n s = if String.length s < n then s else (String.sub s 0 n) ^ "..." in
  let trunc80 s = trunc 80 s in
  let flow_params_to_string params =
    let params = Core_list.map ~f:(fun p -> p.param_name ^ ": " ^ p.param_ty) params in
    "(" ^ (String.concat ", " params) ^ ")"
  in
  let itemType, inlineDetail, detail = match item.func_details with
    | Some func_details ->
      let itemType = Some (trunc 30 func_details.return_ty) in
      let inlineDetail = Some (trunc 40 (flow_params_to_string func_details.param_tys)) in
      let detail = Some (trunc80 item.res_ty) in
      itemType, inlineDetail, detail
    | None ->
      let itemType = None in
      let inlineDetail = Some (trunc80 item.res_ty) in
      let detail = Some (trunc80 item.res_ty) in
      itemType, inlineDetail, detail
  in
  {
    label = item.res_name;
    kind = item.res_kind;
    detail = detail;
    inlineDetail;
    itemType;
    documentation = None; (* This will be filled in by completionItem/resolve. *)
    sortText = None;
    filterText = None;
    insertText = None;
    insertTextFormat = Some PlainText;
    textEdits = [];
    command = None;
    data = None;
  }

let file_key_to_uri (file_key_opt: File_key.t option): (string, string) result =
  let (>>|) = Core_result.(>>|) in
  let (>>=) = Core_result.(>>=) in
  Core_result.of_option file_key_opt ~error:"File_key is None"
    >>= File_key.to_path
    >>| File_url.create

let loc_to_lsp_range (loc: Loc.t): Lsp.range =
  { Lsp.
    start = { Lsp.line=loc.Loc.start.Loc.line-1; character=loc.Loc.start.Loc.column; };
    end_ = { Lsp.line=loc.Loc._end.Loc.line-1; character=loc.Loc._end.Loc.column; };
  }

let loc_to_lsp (loc: Loc.t): (Lsp.Location.t, string) result =
  let (>>|) = Core_result.(>>|) in
  file_key_to_uri loc.Loc.source >>| fun uri -> { Lsp.Location.uri; range = loc_to_lsp_range loc; }

let loc_to_lsp_with_default (loc: Loc.t) ~(default_uri: string): Lsp.Location.t =
  let uri = match file_key_to_uri loc.Loc.source with
    | Ok uri -> uri
    | Error _ -> default_uri
  in
  { Lsp.Location.uri; range = loc_to_lsp_range loc; }

let lsp_position_to_flow (position: Lsp.position): int * int =
  let open Lsp in
  let line = position.line + 1 in
  let char = position.character
  in
  (line, char)

let flow_edit_to_textedit (edit: Loc.t * string): Lsp.TextEdit.t =
  let loc, text = edit in
  { Lsp.TextEdit.range = loc_to_lsp_range loc; newText = text }

let lsp_DocumentIdentifier_to_flow
    (textDocument: Lsp.TextDocumentIdentifier.t)
    ~(client: Persistent_connection.single_client)
  : File_input.t =
  let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
  (* ~, . and .. have no meaning in file urls so we don't canonicalize them *)
  (* but symlinks must be canonicalized before being used in flow: *)
  let fn = Option.value (Sys_utils.realpath fn) ~default:fn in
  let file = Persistent_connection.get_file client fn
  in
  file

let lsp_DocumentPosition_to_flow
    (params: Lsp.TextDocumentPositionParams.t)
    ~(client: Persistent_connection.single_client)
  : File_input.t * int * int =
  let open Lsp.TextDocumentPositionParams in
  let file = lsp_DocumentIdentifier_to_flow params.textDocument client in
  let (line, char) = lsp_position_to_flow params.position
  in
  (file, line, char)



module DocumentSymbols = struct
  let name_of_key (key: (Loc.t, Loc.t) Ast.Expression.Object.Property.key) : string option =
    let open Ast.Expression.Object.Property in
    match key with
    | Literal (_, { Ast.Literal.raw; _ }) -> Some raw
    | Identifier (_, { Ast.Identifier.name= id; comments= _ }) -> Some id
    | PrivateName (_, (_, { Ast.Identifier.name= id; comments= _ })) -> Some id
    | Computed (_, _) -> None

  let name_of_id ((_, { Ast.Identifier.name; comments= _ }): Loc.t Ast.Identifier.t) : string =
    name

  let name_of_id_opt (id_opt: Loc.t Ast.Identifier.t option) : string option =
    Option.map id_opt ~f:name_of_id

  let ast_name
      ~(uri: Lsp.documentUri)
      ~(acc: Lsp.SymbolInformation.t list)
      ~(loc: Loc.t)
      ~(containerName: string option)
      ~(name: string)
      ~(kind: Lsp.SymbolInformation.symbolKind)
    : Lsp.SymbolInformation.t list =
    { Lsp.SymbolInformation.
      name;
      kind;
      location = { Lsp.Location.uri; range = loc_to_lsp_range loc};
      containerName;
    } :: acc

  let ast_name_opt ~uri ~containerName ~acc ~loc ~(name_opt: string option) ~kind =
    Option.value_map name_opt ~default:acc
      ~f:(fun name -> ast_name ~uri ~containerName ~acc ~loc ~name ~kind)

  let ast_key ~uri ~containerName ~acc ~loc ~(key:(Loc.t, Loc.t) Ast.Expression.Object.Property.key) ~kind =
    ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_key key) ~kind

  let ast_id ~uri ~containerName ~acc ~loc ~(id: Loc.t Ast.Identifier.t) ~kind =
    ast_name ~uri ~containerName ~acc ~loc ~name:(name_of_id id) ~kind

  let ast_id_opt ~uri ~containerName ~acc ~loc ~(id_opt: Loc.t Ast.Identifier.t option) ~kind =
    ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_id_opt id_opt) ~kind

  let ast_class_member
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      (acc: Lsp.SymbolInformation.t list)
      (member: (Loc.t, Loc.t) Ast.Class.Body.element)
    : Lsp.SymbolInformation.t list =
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
    | Body.PrivateField (loc, { PrivateField.key = (_, (_, { Ast.Identifier.name; comments= _ })); _ }) ->
      ast_name ~uri ~containerName ~acc ~loc ~name ~kind:Lsp.SymbolInformation.Field

  let ast_class
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      ~(acc: Lsp.SymbolInformation.t list)
      ~(loc: Loc.t)
      ~(class_: (Loc.t, Loc.t) Ast.Class.t)
    : Lsp.SymbolInformation.t list =
    let open Ast.Class in
    let acc = ast_id_opt ~uri ~containerName ~acc
      ~loc ~id_opt:class_.id ~kind:Lsp.SymbolInformation.Class in
    let containerName = name_of_id_opt class_.id in
    let (_, body) = class_.body in
    Core_list.fold body.Body.body ~init:acc ~f:(ast_class_member ~uri ~containerName)

  let ast_type_object_property
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      (acc: Lsp.SymbolInformation.t list)
      (property: (Loc.t, Loc.t) Ast.Type.Object.property)
    : Lsp.SymbolInformation.t list =
    let open Ast.Type.Object in
    match property with
    | Property (loc, { Property.key; _}) ->
      ast_key ~uri ~containerName ~acc ~loc ~key ~kind:Lsp.SymbolInformation.Property
    | Indexer (loc, { Indexer.id; _}) ->
      ast_id_opt ~uri ~containerName ~acc ~loc ~id_opt:id ~kind:Lsp.SymbolInformation.Property
    | InternalSlot (loc, { InternalSlot.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Property
    | _ -> acc

  let ast_type_object
     ~(uri: Lsp.documentUri)
     ~(containerName: string option)
     ~(acc: Lsp.SymbolInformation.t list)
     ~(object_: (Loc.t, Loc.t) Ast.Type.Object.t)
    : Lsp.SymbolInformation.t list =
    let open Ast.Type.Object in
    Core_list.fold object_.properties ~init:acc ~f:(ast_type_object_property ~uri ~containerName)

  let ast_type
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      ~(acc: Lsp.SymbolInformation.t list)
      ~(type_: (Loc.t, Loc.t) Ast.Type.t')
    : Lsp.SymbolInformation.t list =
    let open Ast.Type in
    match type_ with
    | Object object_ ->
      ast_type_object ~uri ~containerName ~acc ~object_
    | Interface {Interface.body=(_, object_); _} ->
      ast_type_object ~uri ~containerName ~acc ~object_
    | _ -> acc

  let ast_statement_declaration
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      ~(acc: Lsp.SymbolInformation.t list)
      ~(declaration: (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.declaration)
    : Lsp.SymbolInformation.t list =
    let open Ast.Statement.DeclareExportDeclaration in
    let open Ast.Statement in
    match declaration with
    | Variable (loc, {DeclareVariable.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Variable
    | Function (loc, {DeclareFunction.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Function
    | Class (loc, {DeclareClass.id; body=(_, object_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
      ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
    | DefaultType (_, type_) ->
      ast_type ~uri ~containerName ~acc ~type_
    | NamedType (loc, {TypeAlias.id; right=(_, type_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
      ast_type ~uri ~containerName:(Some (name_of_id id)) ~acc ~type_
    | NamedOpaqueType (loc, {OpaqueType.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
    | Interface (loc, {Interface.id; body=(_, object_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Interface in
      ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_

  let ast_expression
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      ~(acc: Lsp.SymbolInformation.t list)
      ~(expression: (Loc.t, Loc.t) Ast.Expression.t)
    : Lsp.SymbolInformation.t list =
    let open Ast.Expression in
    match expression with
    | (loc, Class class_) -> ast_class ~uri ~containerName ~acc ~loc ~class_
    | (_, _) -> acc

  let rec ast_statement
      ~(uri: Lsp.documentUri)
      ~(containerName: string option)
      (acc: Lsp.SymbolInformation.t list)
      (statement: (Loc.t, Loc.t) Ast.Statement.t)
    : Lsp.SymbolInformation.t list =
    let open Ast.Statement in
    match statement with
    | (_, Expression {Expression.expression; _}) ->
      ast_expression ~uri ~containerName ~acc ~expression
    | (loc, FunctionDeclaration {Ast.Function.id; _}) ->
      ast_id_opt ~uri ~containerName ~acc ~loc ~id_opt:id ~kind:Lsp.SymbolInformation.Function
    | (loc, ClassDeclaration class_) ->
      ast_class ~uri ~containerName ~acc ~loc ~class_
    | (loc, InterfaceDeclaration {Interface.id; body=(_, object_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Interface in
      ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
    | (_, ExportNamedDeclaration {ExportNamedDeclaration.declaration = Some stmt; _}) ->
      ast_statement ~uri ~containerName acc stmt
    | (_, ExportDefaultDeclaration {
        ExportDefaultDeclaration.declaration = ExportDefaultDeclaration.Declaration stmt; _}) ->
      ast_statement ~uri ~containerName acc stmt
    | (loc, TypeAlias {TypeAlias.id; right=(_, type_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
      ast_type ~uri ~containerName:(Some (name_of_id id)) ~acc ~type_
    | (loc, OpaqueType {OpaqueType.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
    | (_, VariableDeclaration {VariableDeclaration.declarations; kind}) ->
      let kind = match kind with
        | VariableDeclaration.Var -> Lsp.SymbolInformation.Variable
        | VariableDeclaration.Let -> Lsp.SymbolInformation.Variable
        | VariableDeclaration.Const -> Lsp.SymbolInformation.Constant in
      let ast_pattern acc loc (_, pattern) =
        let open Ast.Pattern in
        match pattern with
        | Identifier { Identifier.name; _ } -> ast_id ~uri ~containerName ~acc ~loc ~id:name ~kind
        | _ -> acc in
      let ast_declarator acc (loc, declarator) =
        ast_pattern acc loc declarator.VariableDeclaration.Declarator.id in
      Core_list.fold declarations ~init:acc ~f:ast_declarator
    | (loc, DeclareClass {DeclareClass.id; body=(_, object_); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
      ast_type_object ~uri ~containerName:(Some (name_of_id id)) ~acc ~object_
    | (loc, DeclareFunction {DeclareFunction.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Function
    | (loc, DeclareModule {DeclareModule.id=DeclareModule.Identifier id;
        body=(_,{Block.body}); _}) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Module in
      let containerName = Some (name_of_id id) in
      Core_list.fold body ~init:acc ~f:(ast_statement ~uri ~containerName)
    | (loc, DeclareVariable {DeclareVariable.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Variable
    | (loc, DeclareOpaqueType {OpaqueType.id; _}) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
    | (_, DeclareExportDeclaration {DeclareExportDeclaration.declaration=Some declaration; _}) ->
      ast_statement_declaration ~uri ~containerName ~acc ~declaration
    | _ -> acc
end

let flow_ast_to_lsp_symbols
    ~(uri: Lsp.documentUri)
    (program: (Loc.t, Loc.t) Ast.program)
  : Lsp.SymbolInformation.t list =
  let (_loc, statements, _comments) = program in
  Core_list.fold statements ~init:[] ~f:(DocumentSymbols.ast_statement ~uri ~containerName:None)
