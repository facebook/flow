(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let flow_position_to_lsp (line : int) (char : int) : Lsp.position =
  Lsp.{ line = max 0 (line - 1); character = char }

let lsp_position_to_flow (position : Lsp.position) : int * int =
  Lsp.(
    let line = position.line + 1 in
    let char = position.character in
    (line, char))

let lsp_position_to_flow_position p =
  let (line, column) = lsp_position_to_flow p in
  Loc.{ line; column }

let lsp_range_to_flow_loc ?source (range : Lsp.range) =
  Lsp.
    {
      Loc.source;
      start = lsp_position_to_flow_position range.start;
      _end = lsp_position_to_flow_position range.end_;
    }

let loc_to_lsp_range (loc : Loc.t) : Lsp.range =
  Loc.(
    let loc_start = loc.start in
    let loc_end = loc._end in
    let start = flow_position_to_lsp loc_start.line loc_start.column in
    (* Flow's end range is inclusive, LSP's is exclusive.
     * +1 for that, but -1 to make it 0-based *)
    let end_ = flow_position_to_lsp loc_end.line loc_end.column in
    { Lsp.start; end_ })

let flow_completion_to_lsp
    (is_snippet_supported : bool) (item : ServerProt.Response.complete_autocomplete_result) :
    Lsp.Completion.completionItem =
  Lsp.Completion.(
    ServerProt.Response.(
      let insert_text = Option.value item.res_insert_text ~default:item.res_name in
      let trunc n s =
        if String.length s < n then
          s
        else
          String.sub s 0 n ^ "..."
      in
      let column_width = 80 in
      let flow_params_to_string params =
        let params = Base.List.map ~f:(fun p -> p.param_name ^ ": " ^ p.param_ty) params in
        "(" ^ String.concat ", " params ^ ")"
      in
      let flow_params_to_lsp_snippet name params =
        let params =
          Base.List.mapi
            ~f:(fun i p -> "${" ^ string_of_int (i + 1) ^ ":" ^ p.param_name ^ "}")
            params
        in
        name ^ "(" ^ String.concat ", " params ^ ")"
      in
      let text_edit loc newText : Lsp.TextEdit.t =
        { Lsp.TextEdit.range = loc_to_lsp_range loc; Lsp.TextEdit.newText }
      in
      let func_snippet func_details =
        let newText = flow_params_to_lsp_snippet insert_text func_details.param_tys in
        text_edit item.res_loc newText
      in
      let plaintext_text_edits =
        lazy
          ( if Option.is_some item.res_insert_text then
            [text_edit item.res_loc insert_text]
          else
            [] )
      in
      let (itemType, inlineDetail, detail, insertTextFormat, textEdits) =
        match item.func_details with
        | Some func_details ->
          let itemType = Some (trunc 30 func_details.return_ty) in
          let inlineDetail = Some (trunc 40 (flow_params_to_string func_details.param_tys)) in
          let detail = Some (trunc column_width item.res_ty) in
          let (insertTextFormat, textEdits) =
            match is_snippet_supported with
            | true -> (Some SnippetFormat, [func_snippet func_details])
            | false -> (Some PlainText, Lazy.force plaintext_text_edits)
          in
          (itemType, inlineDetail, detail, insertTextFormat, textEdits)
        | None ->
          let itemType = None in
          let inlineDetail = Some (trunc column_width item.res_ty) in
          let detail = inlineDetail in
          (itemType, inlineDetail, detail, Some PlainText, Lazy.force plaintext_text_edits)
      in
      let sortText = Some (Printf.sprintf "%020u" item.rank) in
      {
        label = item.res_name;
        kind = item.res_kind;
        detail;
        inlineDetail;
        itemType;
        documentation = None;
        (* This will be filled in by completionItem/resolve. *)
        sortText;
        filterText = None;
        (* deprecated and should not be used *)
        insertText = None;
        insertTextFormat;
        textEdits;
        command = None;
        data = None;
      }))

let file_key_to_uri (file_key_opt : File_key.t option) : (string, string) result =
  let ( >>| ) = Base.Result.( >>| ) in
  let ( >>= ) = Base.Result.( >>= ) in
  Base.Result.of_option file_key_opt ~error:"File_key is None"
  >>= File_key.to_path
  >>| File_url.create

let loc_to_lsp (loc : Loc.t) : (Lsp.Location.t, string) result =
  let ( >>| ) = Base.Result.( >>| ) in
  file_key_to_uri loc.Loc.source >>| fun uri ->
  { Lsp.Location.uri = Lsp.uri_of_string uri; range = loc_to_lsp_range loc }

let loc_to_lsp_with_default (loc : Loc.t) ~(default_uri : string) : Lsp.Location.t =
  let uri =
    match file_key_to_uri loc.Loc.source with
    | Ok uri -> uri
    | Error _ -> default_uri
  in
  { Lsp.Location.uri = Lsp.uri_of_string uri; range = loc_to_lsp_range loc }

let flow_edit_to_textedit (edit : Loc.t * string) : Lsp.TextEdit.t =
  let (loc, text) = edit in
  { Lsp.TextEdit.range = loc_to_lsp_range loc; newText = text }

let flow_loc_patch_to_lsp_edits (p : (Loc.t * string) list) : Lsp.TextEdit.t list =
  let convert_edit (loc, text) = { Lsp.TextEdit.range = loc_to_lsp_range loc; newText = text } in
  List.map convert_edit p

(* ~, . and .. have no meaning in file urls so we don't canonicalize them *)
(* but symlinks must be canonicalized before being used in flow: *)
let lsp_DocumentIdentifier_to_flow_path textDocument =
  let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
  Sys_utils.realpath fn |> Option.value ~default:fn

let lsp_DocumentIdentifier_to_flow
    (textDocument : Lsp.TextDocumentIdentifier.t) ~(client : Persistent_connection.single_client) :
    File_input.t =
  lsp_DocumentIdentifier_to_flow_path textDocument |> Persistent_connection.get_file client

let lsp_DocumentPosition_to_flow
    (params : Lsp.TextDocumentPositionParams.t) ~(client : Persistent_connection.single_client) :
    File_input.t * int * int =
  Lsp.TextDocumentPositionParams.(
    let file = lsp_DocumentIdentifier_to_flow params.textDocument client in
    let (line, char) = lsp_position_to_flow params.position in
    (file, line, char))

let lsp_textDocument_and_range_to_flow
    ?(file_key_of_path = (fun p -> File_key.SourceFile p)) td range client =
  let path = lsp_DocumentIdentifier_to_flow_path td in
  let file_key = file_key_of_path path in
  let file = Persistent_connection.get_file client path in
  let loc = lsp_range_to_flow_loc ~source:file_key range in
  (file_key, file, loc)

module DocumentSymbols = struct
  let name_of_key (key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) : string option =
    let open Ast.Expression.Object.Property in
    match key with
    | Literal (_, { Ast.Literal.raw; _ }) -> Some raw
    | Identifier (_, { Ast.Identifier.name = id; comments = _ }) -> Some id
    | PrivateName (_, (_, { Ast.Identifier.name = id; comments = _ })) -> Some id
    | Computed (_, _) -> None

  let name_of_id ((_, { Ast.Identifier.name; comments = _ }) : (Loc.t, Loc.t) Ast.Identifier.t) :
      string =
    name

  let name_of_id_opt (id_opt : (Loc.t, Loc.t) Ast.Identifier.t option) : string option =
    Option.map id_opt ~f:name_of_id

  let ast_name
      ~(uri : Lsp.documentUri)
      ~(acc : Lsp.SymbolInformation.t list)
      ~(loc : Loc.t)
      ~(containerName : string option)
      ~(name : string)
      ~(kind : Lsp.SymbolInformation.symbolKind) : Lsp.SymbolInformation.t list =
    {
      Lsp.SymbolInformation.name;
      kind;
      location = { Lsp.Location.uri; range = loc_to_lsp_range loc };
      containerName;
    }
    :: acc

  let ast_name_opt ~uri ~containerName ~acc ~loc ~(name_opt : string option) ~kind =
    Option.value_map name_opt ~default:acc ~f:(fun name ->
        ast_name ~uri ~containerName ~acc ~loc ~name ~kind)

  let ast_key
      ~uri ~containerName ~acc ~loc ~(key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) ~kind
      =
    ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_key key) ~kind

  let ast_id ~uri ~containerName ~acc ~loc ~(id : (Loc.t, Loc.t) Ast.Identifier.t) ~kind =
    ast_name ~uri ~containerName ~acc ~loc ~name:(name_of_id id) ~kind

  let ast_id_opt
      ~uri ~containerName ~acc ~loc ~(id_opt : (Loc.t, Loc.t) Ast.Identifier.t option) ~kind =
    ast_name_opt ~uri ~containerName ~acc ~loc ~name_opt:(name_of_id_opt id_opt) ~kind

  let ast_class_member
      ~(uri : Lsp.documentUri)
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
        (loc, { PrivateField.key = (_, (_, { Ast.Identifier.name; comments = _ })); _ }) ->
      ast_name ~uri ~containerName ~acc ~loc ~name ~kind:Lsp.SymbolInformation.Field

  let ast_class
      ~(uri : Lsp.documentUri)
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
      ~(uri : Lsp.documentUri)
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
      ~(uri : Lsp.documentUri)
      ~(containerName : string option)
      ~(acc : Lsp.SymbolInformation.t list)
      ~(object_ : (Loc.t, Loc.t) Ast.Type.Object.t) : Lsp.SymbolInformation.t list =
    let open Ast.Type.Object in
    Base.List.fold object_.properties ~init:acc ~f:(ast_type_object_property ~uri ~containerName)

  let ast_type
      ~(uri : Lsp.documentUri)
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
      ~(uri : Lsp.documentUri)
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
      ~(uri : Lsp.documentUri)
      ~(containerName : string option)
      ~(acc : Lsp.SymbolInformation.t list)
      ~(expression : (Loc.t, Loc.t) Ast.Expression.t) : Lsp.SymbolInformation.t list =
    let open Ast.Expression in
    match expression with
    | (loc, Class class_) -> ast_class ~uri ~containerName ~acc ~loc ~class_
    | (_, _) -> acc

  let rec ast_statement
      ~(uri : Lsp.documentUri)
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
          { ExportDefaultDeclaration.declaration = ExportDefaultDeclaration.Declaration stmt; _ } )
      ->
      ast_statement ~uri ~containerName acc stmt
    | (loc, TypeAlias { TypeAlias.id; right = (_, type_); _ }) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class in
      ast_type ~uri ~containerName:(Some (name_of_id id)) ~acc ~type_
    | (loc, OpaqueType { OpaqueType.id; _ }) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
    | (_, VariableDeclaration { VariableDeclaration.declarations; kind }) ->
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
          { DeclareModule.id = DeclareModule.Identifier id; body = (_, { Block.body }); _ } ) ->
      let acc = ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Module in
      let containerName = Some (name_of_id id) in
      Base.List.fold body ~init:acc ~f:(ast_statement ~uri ~containerName)
    | (loc, DeclareVariable { DeclareVariable.id; _ }) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Variable
    | (loc, DeclareOpaqueType { OpaqueType.id; _ }) ->
      ast_id ~uri ~containerName ~acc ~loc ~id ~kind:Lsp.SymbolInformation.Class
    | (_, DeclareExportDeclaration { DeclareExportDeclaration.declaration = Some declaration; _ })
      ->
      ast_statement_declaration ~uri ~containerName ~acc ~declaration
    | _ -> acc
end

let flow_ast_to_lsp_symbols ~(uri : Lsp.documentUri) (program : (Loc.t, Loc.t) Ast.program) :
    Lsp.SymbolInformation.t list =
  let (_loc, statements, _comments) = program in
  Base.List.fold statements ~init:[] ~f:(DocumentSymbols.ast_statement ~uri ~containerName:None)
