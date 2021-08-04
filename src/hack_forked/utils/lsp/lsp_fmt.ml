(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Hh_core
open Lsp
open Hh_json
open Hh_json_helpers

(************************************************************************)
(* Miscellaneous LSP structures                                         *)
(************************************************************************)

let parse_id (json : json) : lsp_id =
  match json with
  | JSON_Number s ->
    begin
      try NumberId (int_of_string s) with
      | Failure _ ->
        raise
          (Error.LspException
             { Error.code = Error.ParseError; message = "float ids not allowed: " ^ s; data = None })
    end
  | JSON_String s -> StringId s
  | _ ->
    raise
      (Error.LspException
         {
           Error.code = Error.ParseError;
           message = "not an id: " ^ Hh_json.json_to_string json;
           data = None;
         })

let parse_id_opt (json : json option) : lsp_id option = Base.Option.map json ~f:parse_id

let print_id (id : lsp_id) : json =
  match id with
  | NumberId n -> JSON_Number (string_of_int n)
  | StringId s -> JSON_String s

let id_to_string (id : lsp_id) : string =
  match id with
  | NumberId n -> string_of_int n
  | StringId s -> Printf.sprintf "\"%s\"" s

let parse_position (json : json option) : position =
  { line = Jget.int_exn json "line"; character = Jget.int_exn json "character" }

let print_position (position : position) : json =
  JSON_Object [("line", position.line |> int_); ("character", position.character |> int_)]

let print_range (range : range) : json =
  JSON_Object [("start", print_position range.start); ("end", print_position range.end_)]

let print_location (location : Location.t) : json =
  Location.(
    JSON_Object
      [
        ("uri", JSON_String (DocumentUri.to_string location.uri));
        ("range", print_range location.range);
      ])

let print_definition_location (definition_location : DefinitionLocation.t) : json =
  DefinitionLocation.(
    let location = definition_location.location in
    Jprint.object_opt
      [
        ("uri", Some (JSON_String (DocumentUri.to_string location.Location.uri)));
        ("range", Some (print_range location.Location.range));
        ("title", Base.Option.map definition_location.title ~f:string_);
      ])

let parse_range_exn (json : json option) : range =
  {
    start = Jget.obj_exn json "start" |> parse_position;
    end_ = Jget.obj_exn json "end" |> parse_position;
  }

let parse_location (j : json option) : Location.t =
  Location.
    {
      uri = Jget.string_exn j "uri" |> DocumentUri.of_string;
      range = Jget.obj_exn j "range" |> parse_range_exn;
    }

let parse_range_opt (json : json option) : range option =
  if json = None then
    None
  else
    Some (parse_range_exn json)

let parse_textDocumentIdentifier (json : json option) : TextDocumentIdentifier.t =
  TextDocumentIdentifier.{ uri = Jget.string_exn json "uri" |> DocumentUri.of_string }

let parse_versionedTextDocumentIdentifier (json : json option) : VersionedTextDocumentIdentifier.t =
  VersionedTextDocumentIdentifier.
    {
      uri = Jget.string_exn json "uri" |> DocumentUri.of_string;
      version = Jget.int_d json "version" 0;
    }

let parse_textDocumentItem (json : json option) : TextDocumentItem.t =
  TextDocumentItem.
    {
      uri = Jget.string_exn json "uri" |> DocumentUri.of_string;
      languageId = Jget.string_d json "languageId" "";
      version = Jget.int_d json "version" 0;
      text = Jget.string_exn json "text";
    }

let print_textDocumentItem (item : TextDocumentItem.t) : json =
  TextDocumentItem.(
    JSON_Object
      [
        ("uri", JSON_String (DocumentUri.to_string item.uri));
        ("languageId", JSON_String item.languageId);
        ("version", JSON_Number (string_of_int item.version));
        ("text", JSON_String item.text);
      ])

let print_markedItem (item : markedString) : json =
  match item with
  | MarkedString s -> JSON_String s
  | MarkedCode (language, value) ->
    JSON_Object [("language", JSON_String language); ("value", JSON_String value)]

let parse_textDocumentPositionParams (params : json option) : TextDocumentPositionParams.t =
  TextDocumentPositionParams.
    {
      textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
      position = Jget.obj_exn params "position" |> parse_position;
    }

let parse_textEdit (params : json option) : TextEdit.t option =
  match params with
  | None -> None
  | _ ->
    TextEdit.(
      Some
        {
          range = Jget.obj_exn params "range" |> parse_range_exn;
          newText = Jget.string_exn params "newText";
        })

let print_textEdit (edit : TextEdit.t) : json =
  TextEdit.(JSON_Object [("range", print_range edit.range); ("newText", JSON_String edit.newText)])

let print_workspaceEdit (r : WorkspaceEdit.t) : json =
  WorkspaceEdit.(
    let print_workspace_edit_changes (uri, text_edits) =
      (DocumentUri.to_string uri, JSON_Array (List.map ~f:print_textEdit text_edits))
    in
    JSON_Object
      [
        ( "changes",
          JSON_Object (List.map (UriMap.elements r.changes) ~f:print_workspace_edit_changes) );
      ])

let print_command_name ~key name =
  match name with
  | Command.Command name -> Printf.sprintf "%s:%s" name key

let print_command ~key (command : Command.t) : json =
  let open Command in
  let name = print_command_name ~key command.command in
  JSON_Object
    [
      ("title", JSON_String command.title);
      ("command", JSON_String name);
      ("arguments", JSON_Array command.arguments);
    ]

let parse_command_name str =
  let delim = String.index str ':' in
  Lsp.Command.Command (String.sub str 0 delim)

let parse_command (json : json option) : Command.t =
  let open Command in
  let name = Jget.string_d json "command" "" in
  {
    title = Jget.string_d json "title" "";
    command = parse_command_name name;
    arguments = Jget.array_d json "arguments" ~default:[] |> List.filter_opt;
  }

let parse_formattingOptions (json : json option) : DocumentFormatting.formattingOptions =
  {
    DocumentFormatting.tabSize = Jget.int_d json "tabSize" 2;
    insertSpaces = Jget.bool_d json "insertSpaces" true;
  }

let print_symbolInformation (info : SymbolInformation.t) : json =
  SymbolInformation.(
    let print_symbol_kind k = int_ (SymbolInformation.symbolKind_to_enum k) in
    Jprint.object_opt
      [
        ("name", Some (JSON_String info.name));
        ("kind", Some (print_symbol_kind info.kind));
        ("location", Some (print_location info.location));
        ("containerName", Base.Option.map info.containerName string_);
      ])

let parse_codeLens (json : json option) : CodeLens.t =
  CodeLens.
    {
      range = Jget.obj_exn json "range" |> parse_range_exn;
      command = Jget.obj_exn json "command" |> parse_command;
      data = Jget.obj_exn json "data";
    }

let print_codeLens ~key (codeLens : CodeLens.t) : json =
  CodeLens.(
    JSON_Object
      [
        ("range", print_range codeLens.range);
        ("command", print_command ~key codeLens.command);
        ( "data",
          match codeLens.data with
          | None -> JSON_Null
          | Some json -> json );
      ])

module MarkupKindFmt = struct
  open MarkupKind

  let to_string = function
    | Markdown -> "markdown"
    | PlainText -> "plaintext"

  let to_json kind = Hh_json.JSON_String (to_string kind)

  let of_string_opt = function
    | "markdown" -> Some Markdown
    | "plaintext" -> Some PlainText
    | _ -> None

  let of_json = function
    | JSON_String str -> of_string_opt str
    | _ -> None
end

module MarkupContentFmt = struct
  open MarkupContent

  let to_json { kind; value } =
    Hh_json.JSON_Object [("kind", MarkupKindFmt.to_json kind); ("value", Hh_json.JSON_String value)]
end

(************************************************************************)
(* shutdown request                                                     *)
(************************************************************************)

let print_shutdown () : json = JSON_Null

(************************************************************************)
(* $/cancelRequest notification                                         *)
(************************************************************************)

let parse_cancelRequest (params : json option) : CancelRequest.params =
  CancelRequest.{ id = Jget.val_exn params "id" |> parse_id }

let print_cancelRequest (p : CancelRequest.params) : json =
  CancelRequest.(JSON_Object [("id", print_id p.id)])

(************************************************************************)
(* rage request                                                         *)
(************************************************************************)

let print_rage (r : Rage.result) : json =
  Rage.(
    let print_item (item : rageItem) : json =
      JSON_Object
        [
          ("data", JSON_String item.data);
          ( "title",
            match item.title with
            | None -> JSON_Null
            | Some s -> JSON_String s );
        ]
    in
    JSON_Array (List.map r ~f:print_item))

(************************************************************************)
(* textDocument/didOpen notification                                    *)
(************************************************************************)

let parse_didOpen (params : json option) : DidOpen.params =
  DidOpen.{ textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentItem }

let print_didOpen (params : DidOpen.params) : json =
  DidOpen.(JSON_Object [("textDocument", params.textDocument |> print_textDocumentItem)])

(************************************************************************)
(* textDocument/didClose notification                                   *)
(************************************************************************)

let parse_didClose (params : json option) : DidClose.params =
  DidClose.{ textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier }

(************************************************************************)
(* textDocument/didSave notification                                    *)
(************************************************************************)

let parse_didSave (params : json option) : DidSave.params =
  DidSave.
    {
      textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
      text = Jget.string_opt params "text";
    }

(************************************************************************)
(* textDocument/didChange notification                                  *)
(************************************************************************)

let parse_didChange (params : json option) : DidChange.params =
  DidChange.(
    let parse_textDocumentContentChangeEvent json =
      {
        range = Jget.obj_opt json "range" |> parse_range_opt;
        rangeLength = Jget.int_opt json "rangeLength";
        text = Jget.string_exn json "text";
      }
    in
    {
      textDocument = Jget.obj_exn params "textDocument" |> parse_versionedTextDocumentIdentifier;
      contentChanges =
        Jget.array_d params "contentChanges" ~default:[]
        |> List.map ~f:parse_textDocumentContentChangeEvent;
    })

module SelectionRangeFmt = struct
  open SelectionRange
  open Hh_json

  let params_of_json (json : json option) : params =
    {
      textDocument = Jget.obj_exn json "textDocument" |> parse_textDocumentIdentifier;
      positions = Jget.array_d json "positions" ~default:[] |> List.map ~f:parse_position;
    }

  let rec json_of_selection_range { range; parent } =
    Jprint.object_opt
      [("range", Some (print_range range)); ("parent", Option.map json_of_selection_range parent)]

  let json_of_result (t : result) : json =
    let ranges = List.map ~f:json_of_selection_range t in
    JSON_Array ranges
end

(************************************************************************)
(* textDocument/signatureHelp notification                              *)
(************************************************************************)

module SignatureHelpFmt = struct
  open SignatureHelp
  open Hh_json

  let json_of_label : label -> json = function
    | String str -> JSON_String str
    | Offset (start, end_) ->
      JSON_Array [JSON_Number (string_of_int start); JSON_Number (string_of_int end_)]

  let json_of_documentation (doc : Documentation.t) : json =
    match doc with
    | Documentation.String str -> JSON_String str
    | Documentation.MarkupContent content -> MarkupContentFmt.to_json content

  let json_of_parameter { parinfo_label; parinfo_documentation } =
    Jprint.object_opt
      [
        ("label", Some (json_of_label parinfo_label));
        ("documentation", Base.Option.map ~f:json_of_documentation parinfo_documentation);
      ]

  let json_of_signature { siginfo_label; siginfo_documentation; parameters } =
    Jprint.object_opt
      [
        ("label", Some (JSON_String siginfo_label));
        ("documentation", Base.Option.map ~f:json_of_documentation siginfo_documentation);
        ("parameters", Some (JSON_Array (List.map ~f:json_of_parameter parameters)));
      ]

  let to_json (r : SignatureHelp.result) : json =
    match r with
    | None -> JSON_Null
    | Some r ->
      JSON_Object
        [
          ("signatures", JSON_Array (List.map ~f:json_of_signature r.signatures));
          ("activeSignature", int_ r.activeSignature);
          ("activeParameter", int_ r.activeParameter);
        ]

  let context_of_json json : SignatureHelp.context =
    {
      triggerKind =
        Jget.int_opt json "triggerKind"
        |> Base.Option.bind ~f:TriggerKind.of_enum
        |> Base.Option.value ~default:TriggerKind.Invoked;
      triggerCharacter = Jget.string_opt json "triggerCharacter";
      isRetrigger = Jget.bool_d json "isRetrigger" ~default:false;
      activeSignatureHelp = None (* TODO *);
    }

  let of_json json : SignatureHelp.params =
    {
      loc = parse_textDocumentPositionParams json;
      context =
        ( Jget.obj_opt json "context" |> fun json ->
          match json with
          | None -> None
          | Some _ -> Some (context_of_json json) );
    }
end

(************************************************************************)
(* codeLens/resolve Request                                             *)
(************************************************************************)

let parse_codeLensResolve (params : json option) : CodeLensResolve.params = parse_codeLens params

let print_codeLensResolve ~key (r : CodeLensResolve.result) : json = print_codeLens ~key r

(************************************************************************)
(* textDocument/rename Request                                          *)
(************************************************************************)

let parse_documentRename (params : json option) : Rename.params =
  Rename.
    {
      textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
      position = Jget.obj_exn params "position" |> parse_position;
      newName = Jget.string_exn params "newName";
    }

let print_documentRename : Rename.result -> json = print_workspaceEdit

(************************************************************************)
(* textDocument/codeLens Request                                        *)
(************************************************************************)

let parse_documentCodeLens (params : json option) : DocumentCodeLens.params =
  DocumentCodeLens.
    { textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier }

let print_documentCodeLens ~key (r : DocumentCodeLens.result) : json =
  JSON_Array (List.map r ~f:(print_codeLens ~key))

(************************************************************************)
(* workspace/executeCommand Request                                     *)
(************************************************************************)

let parse_executeCommand (params : json option) : ExecuteCommand.params =
  ExecuteCommand.
    {
      command = Jget.string_exn params "command" |> parse_command_name;
      arguments =
        Jget.array_opt params "arguments"
        |> Base.Option.map ~f:(Base.List.map ~f:(fun j -> Base.Option.value_exn j));
    }

let print_executeCommand (() : ExecuteCommand.result) : json = JSON_Null

(************************************************************************)
(* textDocument/publishDiagnostics notification                         *)
(************************************************************************)

let print_diagnostic (diagnostic : PublishDiagnostics.diagnostic) : json =
  PublishDiagnostics.(
    let print_diagnosticSeverity = Fn.compose int_ diagnosticSeverity_to_enum in
    let print_diagnosticCode = function
      | IntCode i -> Some (int_ i)
      | StringCode s -> Some (string_ s)
      | NoCode -> None
    in
    let print_related (related : relatedLocation) : json =
      Hh_json.JSON_Object
        [
          ("location", print_location related.relatedLocation);
          ("message", string_ related.relatedMessage);
        ]
    in
    Jprint.object_opt
      [
        ("range", Some (print_range diagnostic.range));
        ("severity", Base.Option.map diagnostic.severity print_diagnosticSeverity);
        ("code", print_diagnosticCode diagnostic.code);
        ("source", Base.Option.map diagnostic.source string_);
        ("message", Some (JSON_String diagnostic.message));
        ( "relatedInformation",
          Some (JSON_Array (List.map diagnostic.relatedInformation ~f:print_related)) );
        ( "relatedLocations",
          Some (JSON_Array (List.map diagnostic.relatedLocations ~f:print_related)) );
      ])

let print_diagnostic_list (ds : PublishDiagnostics.diagnostic list) : json =
  JSON_Array (List.map ds ~f:print_diagnostic)

let print_diagnostics (r : PublishDiagnostics.params) : json =
  PublishDiagnostics.(
    JSON_Object
      [
        ("uri", JSON_String (DocumentUri.to_string r.uri));
        ("diagnostics", print_diagnostic_list r.diagnostics);
      ])

let parse_diagnostic (j : json option) : PublishDiagnostics.diagnostic =
  PublishDiagnostics.(
    let parse_code = function
      | None -> NoCode
      | Some (JSON_String s) -> StringCode s
      | Some (JSON_Number s) ->
        begin
          try IntCode (int_of_string s) with
          | Failure _ ->
            raise
              (Error.LspException
                 {
                   Error.code = Error.ParseError;
                   message = "Diagnostic code expected to be an int: " ^ s;
                   data = None;
                 })
        end
      | _ ->
        raise
          (Error.LspException
             {
               Error.code = Error.ParseError;
               message = "Diagnostic code expected to be an int or string";
               data = None;
             })
    in
    let parse_info j =
      {
        relatedLocation = Jget.obj_exn j "location" |> parse_location;
        relatedMessage = Jget.string_exn j "message";
      }
    in
    {
      range = Jget.obj_exn j "range" |> parse_range_exn;
      severity =
        Jget.int_opt j "severity"
        |> Base.Option.map ~f:diagnosticSeverity_of_enum
        |> Base.Option.join;
      code = Jget.val_opt j "code" |> parse_code;
      source = Jget.string_opt j "source";
      message = Jget.string_exn j "message";
      relatedInformation = Jget.array_d j "relatedInformation" ~default:[] |> List.map ~f:parse_info;
      relatedLocations = Jget.array_d j "relatedLocations" ~default:[] |> List.map ~f:parse_info;
    })

let parse_kind json : CodeActionKind.t option =
  CodeActionKind.(
    match json with
    | Some (JSON_String s) -> Some (kind_of_string s)
    | _ -> None)

let parse_kinds jsons : CodeActionKind.t list = List.map ~f:parse_kind jsons |> List.filter_opt

let parse_codeActionRequest (j : json option) : CodeActionRequest.params =
  CodeActionRequest.(
    let parse_context c : CodeActionRequest.codeActionContext =
      {
        diagnostics = Jget.array_exn c "diagnostics" |> List.map ~f:parse_diagnostic;
        only = Jget.array_opt c "only" |> Base.Option.map ~f:parse_kinds;
      }
    in
    {
      textDocument = Jget.obj_exn j "textDocument" |> parse_textDocumentIdentifier;
      range = Jget.obj_exn j "range" |> parse_range_exn;
      context = Jget.obj_exn j "context" |> parse_context;
    })

(************************************************************************)
(* textDocument/CodeAction result                                       *)
(************************************************************************)

let print_codeAction ~key (c : CodeAction.t) : json =
  CodeAction.(
    let (edit, command) =
      match c.action with
      | EditOnly e -> (Some e, None)
      | CommandOnly c -> (None, Some c)
      | BothEditThenCommand (e, c) -> (Some e, Some c)
    in
    Jprint.object_opt
      [
        ("title", Some (JSON_String c.title));
        ("kind", Some (JSON_String (CodeActionKind.string_of_kind c.kind)));
        ("diagnostics", Some (print_diagnostic_list c.diagnostics));
        ("edit", Base.Option.map edit ~f:print_documentRename);
        ("command", Base.Option.map command ~f:(print_command ~key));
      ])

let print_codeActionResult ~key (c : CodeAction.result) : json =
  CodeAction.(
    let print_command_or_action = function
      | Command c -> print_command ~key c
      | Action c -> print_codeAction ~key c
    in
    JSON_Array (List.map c ~f:print_command_or_action))

(* print_codeAction *)

(************************************************************************)
(* window/logMessage notification                                       *)
(************************************************************************)

let print_logMessage (type_ : MessageType.t) (message : string) : json =
  LogMessage.(
    let r = { type_; message } in
    JSON_Object [("type", int_ (MessageType.to_enum r.type_)); ("message", JSON_String r.message)])

(************************************************************************)
(* window/showMessage notification                                      *)
(************************************************************************)

let print_showMessage (type_ : MessageType.t) (message : string) : json =
  ShowMessage.(
    let r = { type_; message } in
    JSON_Object [("type", int_ (MessageType.to_enum r.type_)); ("message", JSON_String r.message)])

(************************************************************************)
(* window/showMessage request                                           *)
(************************************************************************)

let print_showMessageRequest (r : ShowMessageRequest.showMessageRequestParams) : json =
  let print_action (action : ShowMessageRequest.messageActionItem) : json =
    JSON_Object [("title", JSON_String action.ShowMessageRequest.title)]
  in
  Jprint.object_opt
    [
      ("type", Some (int_ (MessageType.to_enum r.ShowMessageRequest.type_)));
      ("message", Some (JSON_String r.ShowMessageRequest.message));
      ("actions", Some (JSON_Array (List.map r.ShowMessageRequest.actions ~f:print_action)));
    ]

let parse_result_showMessageRequest (result : json option) : ShowMessageRequest.result =
  ShowMessageRequest.(
    let title = Jget.string_opt result "title" in
    Base.Option.map title ~f:(fun title -> { title }))

(************************************************************************)
(* window/showStatus request                                            *)
(************************************************************************)

let print_showStatus (r : ShowStatus.showStatusParams) : json =
  let print_action (action : ShowMessageRequest.messageActionItem) : json =
    JSON_Object [("title", JSON_String action.ShowMessageRequest.title)]
  in
  let rr = r.ShowStatus.request in
  Jprint.object_opt
    [
      ("type", Some (int_ (MessageType.to_enum rr.ShowMessageRequest.type_)));
      ("actions", Some (JSON_Array (List.map rr.ShowMessageRequest.actions ~f:print_action)));
      ("message", Some (JSON_String rr.ShowMessageRequest.message));
      ("shortMessage", Base.Option.map r.ShowStatus.shortMessage ~f:string_);
      ( "progress",
        Base.Option.map r.ShowStatus.progress ~f:(fun progress ->
            Jprint.object_opt
              [
                ("numerator", Some (int_ progress));
                ("denominator", Base.Option.map r.ShowStatus.total ~f:int_);
              ]) );
    ]

(************************************************************************)
(* telemetry/connectionStatus notification                              *)
(************************************************************************)

let print_connectionStatus (p : ConnectionStatus.params) : json =
  ConnectionStatus.(JSON_Object [("isConnected", JSON_Bool p.isConnected)])

(************************************************************************)
(* textDocument/hover request                                           *)
(************************************************************************)

let parse_hover (params : json option) : Hover.params = parse_textDocumentPositionParams params

let print_hover (r : Hover.result) : json =
  Hover.(
    match r with
    | None -> JSON_Null
    | Some r ->
      Jprint.object_opt
        [
          ("contents", Some (JSON_Array (List.map r.Hover.contents ~f:print_markedItem)));
          ("range", Base.Option.map r.range ~f:print_range);
        ])

(************************************************************************)
(* textDocument/definition request                                      *)
(************************************************************************)

let parse_definition (params : json option) : Definition.params =
  parse_textDocumentPositionParams params

let print_definition (r : Definition.result) : json =
  JSON_Array (List.map r ~f:print_definition_location)

(************************************************************************)
(* completionItem/resolve request                                       *)
(************************************************************************)

module CompletionItemLabelDetailsFmt = struct
  open CompletionItemLabelDetails

  let of_json json : CompletionItemLabelDetails.t =
    let json = Some json in
    let parameters = Jget.string_opt json "parameters" in
    let qualifier = Jget.string_opt json "qualifier" in
    let type_ = Jget.string_opt json "type" in
    { parameters; qualifier; type_ }

  let to_json { parameters; qualifier; type_ } =
    Jprint.object_opt
      [
        ("parameters", Base.Option.map ~f:(fun x -> JSON_String x) parameters);
        ("qualifier", Base.Option.map ~f:(fun x -> JSON_String x) qualifier);
        ("type", Base.Option.map ~f:(fun x -> JSON_String x) type_);
      ]
end

let parse_completionItem (params : json option) : CompletionItemResolve.params =
  Completion.(
    let textEdits =
      Jget.obj_opt params "textEdit" :: Jget.array_d params "additionalTextEdits" ~default:[]
      |> List.filter_map ~f:parse_textEdit
    in
    let command =
      match Jget.obj_opt params "command" with
      | None -> None
      | c -> Some (parse_command c)
    in
    {
      label = Jget.string_exn params "label";
      labelDetails =
        Base.Option.map (Jget.obj_opt params "labelDetails") CompletionItemLabelDetailsFmt.of_json;
      kind = Base.Option.bind (Jget.int_opt params "kind") completionItemKind_of_enum;
      detail = Jget.string_opt params "detail";
      documentation = None;
      preselect = Jget.bool_d params "preselect" ~default:false;
      sortText = Jget.string_opt params "sortText";
      filterText = Jget.string_opt params "filterText";
      insertText = Jget.string_opt params "insertText";
      insertTextFormat =
        Base.Option.bind (Jget.int_opt params "insertTextFormat") insertTextFormat_of_enum;
      textEdits;
      command;
      data = Jget.obj_opt params "data";
    })

let string_of_markedString (acc : string) (marked : markedString) : string =
  match marked with
  | MarkedCode (lang, code) -> acc ^ "```" ^ lang ^ "\n" ^ code ^ "\n" ^ "```\n"
  | MarkedString str -> acc ^ str ^ "\n"

let print_completionItem ~key (item : Completion.completionItem) : json =
  Completion.(
    Jprint.object_opt
      [
        ("label", Some (JSON_String item.label));
        ("labelDetails", Base.Option.map item.labelDetails ~f:CompletionItemLabelDetailsFmt.to_json);
        ("kind", Base.Option.map item.kind (fun x -> int_ @@ completionItemKind_to_enum x));
        ("detail", Base.Option.map item.detail string_);
        ( "documentation",
          Base.Option.map item.documentation ~f:(fun doc ->
              JSON_Object
                [
                  ("kind", JSON_String "markdown");
                  ( "value",
                    JSON_String (String.trim (List.fold doc ~init:"" ~f:string_of_markedString)) );
                ]) );
        ( "preselect",
          if item.preselect then
            Some (JSON_Bool true)
          else
            None );
        ("sortText", Base.Option.map item.sortText string_);
        ("filterText", Base.Option.map item.filterText string_);
        ("insertText", Base.Option.map item.insertText string_);
        ( "insertTextFormat",
          Base.Option.map item.insertTextFormat (fun x -> int_ @@ insertTextFormat_to_enum x) );
        ("textEdit", Base.Option.map (List.hd item.textEdits) print_textEdit);
        ( "additionalTextEdits",
          match List.tl item.textEdits with
          | None
          | Some [] ->
            None
          | Some l -> Some (JSON_Array (List.map l ~f:print_textEdit)) );
        ("command", Base.Option.map item.command (print_command ~key));
        ("data", item.data);
      ])

(************************************************************************)
(* textDocument/completion request                                      *)
(************************************************************************)

let parse_completion (params : json option) : Completion.params =
  Lsp.Completion.(
    let context = Jget.obj_opt params "context" in
    {
      loc = parse_textDocumentPositionParams params;
      context =
        (match context with
        | Some _ ->
          let tk = Jget.int_exn context "triggerKind" in
          Some
            {
              triggerKind =
                Base.Option.value_exn
                  ~message:(Printf.sprintf "Unsupported trigger kind: %d" tk)
                  (Lsp.Completion.completionTriggerKind_of_enum tk);
              triggerCharacter = Jget.string_opt context "triggerCharacter";
            }
        | None -> None);
    })

let print_completion ~key (r : Completion.result) : json =
  Completion.(
    JSON_Object
      [
        ("isIncomplete", JSON_Bool r.isIncomplete);
        ("items", JSON_Array (List.map r.items ~f:(print_completionItem ~key)));
      ])

(** workspace/configuration request *)
module ConfigurationFmt = struct
  open Lsp.Configuration

  let item_of_json json =
    {
      scope_uri = Base.Option.map ~f:DocumentUri.of_string (Jget.string_opt json "scopeUri");
      section = Jget.string_opt json "section";
    }

  let json_of_item { scope_uri; section } =
    let scope_uri =
      Base.Option.map ~f:(fun uri -> JSON_String (DocumentUri.to_string uri)) scope_uri
    in
    let section = Base.Option.map ~f:(fun section -> JSON_String section) section in
    Jprint.object_opt [("scopeUri", scope_uri); ("section", section)]

  let params_of_json json =
    let items =
      Jget.array_opt json "items"
      |> Base.Option.value_map ~default:[] ~f:(Base.List.map ~f:item_of_json)
    in
    { items }

  let json_of_params { items } =
    JSON_Object [("items", JSON_Array (Base.List.map ~f:json_of_item items))]

  let result_of_json json =
    match json with
    | Some (JSON_Array items) -> items
    | _ -> []

  let json_of_result items = JSON_Array items
end

(** workspace/didChangeConfiguration notification *)
module DidChangeConfigurationFmt = struct
  open Lsp.DidChangeConfiguration

  let params_of_json json =
    let settings = Jget.val_opt json "settings" |> Base.Option.value ~default:(JSON_Object []) in
    { settings }
end

(************************************************************************)
(* workspace/symbol request                                             *)
(************************************************************************)

let parse_workspaceSymbol (params : json option) : WorkspaceSymbol.params =
  WorkspaceSymbol.{ query = Jget.string_exn params "query" }

let print_workspaceSymbol (r : WorkspaceSymbol.result) : json =
  JSON_Array (List.map r ~f:print_symbolInformation)

(************************************************************************)
(* textDocument/documentSymbol request                                  *)
(************************************************************************)

let parse_documentSymbol (params : json option) : DocumentSymbol.params =
  DocumentSymbol.
    { textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier }

let print_documentSymbol (r : DocumentSymbol.result) : json =
  JSON_Array (List.map r ~f:print_symbolInformation)

(************************************************************************)
(* textDocument/references request                                      *)
(************************************************************************)

let parse_findReferences (params : json option) : FindReferences.params =
  let context = Jget.obj_opt params "context" in
  {
    FindReferences.loc = parse_textDocumentPositionParams params;
    context =
      {
        FindReferences.includeDeclaration = Jget.bool_d context "includeDeclaration" true;
        includeIndirectReferences = Jget.bool_d context "includeIndirectReferences" false;
      };
  }

(************************************************************************)
(* textDocument/implementation request                                  *)
(************************************************************************)

let parse_goToImplementation (params : json option) : GoToImplementation.params =
  { GoToImplementation.loc = parse_textDocumentPositionParams params }

(************************************************************************)
(* shared by textDocument/references and textDocument/implementation    *)
(************************************************************************)

let print_Locations (r : Location.t list) : json = JSON_Array (List.map r ~f:print_location)

(************************************************************************)
(* textDocument/documentHighlight request                               *)
(************************************************************************)

let parse_documentHighlight (params : json option) : DocumentHighlight.params =
  parse_textDocumentPositionParams params

let print_documentHighlight (r : DocumentHighlight.result) : json =
  DocumentHighlight.(
    let print_highlightKind kind = int_ (documentHighlightKind_to_enum kind) in
    let print_highlight highlight =
      Jprint.object_opt
        [
          ("range", Some (print_range highlight.range));
          ("kind", Base.Option.map highlight.kind ~f:print_highlightKind);
        ]
    in
    JSON_Array (List.map r ~f:print_highlight))

(************************************************************************)
(* textDocument/typeCoverage request                                    *)
(************************************************************************)

let parse_typeCoverage (params : json option) : TypeCoverage.params =
  { TypeCoverage.textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier }

let print_typeCoverage (r : TypeCoverage.result) : json =
  TypeCoverage.(
    let print_uncov (uncov : uncoveredRange) : json =
      Jprint.object_opt
        [
          ("range", Some (print_range uncov.range));
          ("message", Base.Option.map uncov.message ~f:string_);
        ]
    in
    JSON_Object
      [
        ("coveredPercent", int_ r.coveredPercent);
        ("uncoveredRanges", JSON_Array (List.map r.uncoveredRanges ~f:print_uncov));
        ("defaultMessage", JSON_String r.defaultMessage);
      ])

(************************************************************************)
(* workspace/toggleTypeCoverage request                                 *)
(************************************************************************)
let parse_toggleTypeCoverage (params : json option) : ToggleTypeCoverage.params =
  { ToggleTypeCoverage.toggle = Jget.bool_d params "toggle" ~default:false }

(************************************************************************)
(* textDocument/formatting request                                      *)
(************************************************************************)

let parse_documentFormatting (params : json option) : DocumentFormatting.params =
  {
    DocumentFormatting.textDocument =
      Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentFormatting (r : DocumentFormatting.result) : json =
  JSON_Array (List.map r ~f:print_textEdit)

(************************************************************************)
(* textDocument/rangeFormatting request                                 *)
(************************************************************************)

let parse_documentRangeFormatting (params : json option) : DocumentRangeFormatting.params =
  {
    DocumentRangeFormatting.textDocument =
      Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
    range = Jget.obj_exn params "range" |> parse_range_exn;
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentRangeFormatting (r : DocumentRangeFormatting.result) : json =
  JSON_Array (List.map r ~f:print_textEdit)

(************************************************************************)
(* textDocument/onTypeFormatting request                                *)
(************************************************************************)

let parse_documentOnTypeFormatting (params : json option) : DocumentOnTypeFormatting.params =
  {
    DocumentOnTypeFormatting.textDocument =
      Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
    position = Jget.obj_exn params "position" |> parse_position;
    ch = Jget.string_exn params "ch";
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentOnTypeFormatting (r : DocumentOnTypeFormatting.result) : json =
  JSON_Array (List.map r ~f:print_textEdit)

(************************************************************************)
(* initialize request                                                   *)
(************************************************************************)

module CodeActionClientCapabilitiesFmt = struct
  open CodeActionClientCapabilities

  module CodeActionLiteralSupportFmt = struct
    open CodeActionLiteralSupport

    let codeActionKind_of_json json =
      Jget.array_opt json "valueSet" |> Base.Option.map ~f:(fun ls -> { valueSet = parse_kinds ls })

    let of_json json = Jget.obj_opt json "codeActionKind" |> codeActionKind_of_json
  end

  let of_json json =
    {
      dynamicRegistration = Jget.bool_d json "dynamicRegistration" ~default:false;
      codeActionLiteralSupport =
        Jget.obj_opt json "codeActionLiteralSupport" |> CodeActionLiteralSupportFmt.of_json;
    }
end

module SelectionRangeClientCapabilitiesFmt = struct
  open SelectionRangeClientCapabilities

  let of_json json = { dynamicRegistration = Jget.bool_d json "dynamicRegistration" ~default:false }
end

module SignatureHelpClientCapabilitiesFmt = struct
  open SignatureHelpClientCapabilities

  let documentationFormat_of_json json =
    Base.Option.value_map json ~default:[] ~f:(fun formats ->
        List.map formats ~f:(Base.Option.bind ~f:MarkupKindFmt.of_json) |> List.filter_opt)

  let parameterInformation_of_json json =
    { labelOffsetSupport = Jget.bool_d json "labelOffsetSupport" ~default:false }

  let signatureInformation_of_json json =
    {
      documentationFormat = Jget.array_opt json "documentationFormat" |> documentationFormat_of_json;
      parameterInformation =
        Jget.obj_opt json "parameterInformation" |> parameterInformation_of_json;
    }

  let of_json json =
    {
      dynamicRegistration = Jget.bool_d json "dynamicRegistration" ~default:false;
      signatureInformation =
        Jget.obj_opt json "signatureInformation" |> signatureInformation_of_json;
      contextSupport = Jget.bool_d json "contextSupport" ~default:false;
    }
end

module CompletionOptionsFmt = struct
  open CompletionOptions

  let completionItem_to_json { labelDetailsSupport } =
    JSON_Object [("labelDetailsSupport", JSON_Bool labelDetailsSupport)]

  let to_json { resolveProvider; triggerCharacters; completionItem } =
    JSON_Object
      [
        ("resolveProvider", JSON_Bool resolveProvider);
        ("triggerCharacters", Jprint.string_array triggerCharacters);
        ("completionItem", completionItem_to_json completionItem);
      ]
end

let parse_initialize (params : json option) : Initialize.params =
  Initialize.(
    let rec parse_initialize json =
      {
        processId = Jget.int_opt json "processId";
        rootPath = Jget.string_opt json "rootPath";
        rootUri = Base.Option.map ~f:DocumentUri.of_string (Jget.string_opt json "rootUri");
        initializationOptions =
          Jget.obj_opt json "initializationOptions" |> parse_initializationOptions;
        client_capabilities = Jget.obj_opt json "capabilities" |> parse_capabilities;
        trace = Jget.string_opt json "trace" |> parse_trace;
      }
    and parse_trace (s : string option) : trace =
      match s with
      | Some "messages" -> Messages
      | Some "verbose" -> Verbose
      | _ -> Off
    and parse_initializationOptions json =
      { liveSyntaxErrors = Jget.bool_d json "liveSyntaxErrors" ~default:true }
    and parse_capabilities json =
      {
        workspace = Jget.obj_opt json "workspace" |> parse_workspace;
        textDocument = Jget.obj_opt json "textDocument" |> parse_textDocument;
        window = Jget.obj_opt json "window" |> parse_window;
        telemetry = Jget.obj_opt json "telemetry" |> parse_telemetry;
        experimental = Jget.obj_opt json "experimental" |> parse_experimental;
      }
    and parse_workspace json =
      {
        applyEdit = Jget.bool_d json "applyEdit" ~default:false;
        configuration = Jget.bool_d json "configuration" ~default:false;
        workspaceEdit = Jget.obj_opt json "workspaceEdit" |> parse_workspaceEdit;
        didChangeConfiguration =
          Jget.obj_opt json "didChangeConfiguration" |> parse_dynamicRegistration;
        didChangeWatchedFiles =
          Jget.obj_opt json "didChangeWatchedFiles" |> parse_dynamicRegistration;
      }
    and parse_dynamicRegistration json =
      { dynamicRegistration = Jget.bool_d json "dynamicRegistration" ~default:false }
    and parse_workspaceEdit json =
      { documentChanges = Jget.bool_d json "documentChanges" ~default:false }
    and parse_textDocument json =
      {
        synchronization = Jget.obj_opt json "synchronization" |> parse_synchronization;
        completion = Jget.obj_opt json "completion" |> parse_completion;
        codeAction = Jget.obj_opt json "codeAction" |> CodeActionClientCapabilitiesFmt.of_json;
        selectionRange =
          Jget.obj_opt json "selectionRange" |> SelectionRangeClientCapabilitiesFmt.of_json;
        signatureHelp =
          Jget.obj_opt json "signatureHelp" |> SignatureHelpClientCapabilitiesFmt.of_json;
      }
    and parse_synchronization json =
      {
        can_willSave = Jget.bool_d json "willSave" ~default:false;
        can_willSaveWaitUntil = Jget.bool_d json "willSaveWaitUntil" ~default:false;
        can_didSave = Jget.bool_d json "didSave" ~default:false;
      }
    and parse_completion json =
      { completionItem = Jget.obj_opt json "completionItem" |> parse_completionItem }
    and parse_completionItem json =
      {
        snippetSupport = Jget.bool_d json "snippetSupport" ~default:false;
        preselectSupport = Jget.bool_d json "preselectSupport" ~default:false;
        labelDetailsSupport = Jget.bool_d json "labelDetailsSupport" ~default:false;
      }
    and parse_window json = { status = Jget.obj_opt json "status" |> Base.Option.is_some }
    and parse_telemetry json =
      { connectionStatus = Jget.obj_opt json "connectionStatus" |> Base.Option.is_some }
    and parse_experimental json =
      { snippetTextEdit = Jget.bool_d json "snippetTextEdit" ~default:false }
    in
    parse_initialize params)

let print_initialize ~key (r : Initialize.result) : json =
  Initialize.(
    let print_textDocumentSyncKind kind = int_ (textDocumentSyncKind_to_enum kind) in
    let cap = r.server_capabilities in
    let sync = cap.textDocumentSync in
    let experimental =
      let { server_snippetTextEdit } = cap.server_experimental in
      let props = [] in
      let props =
        if server_snippetTextEdit then
          ("snippetTextEdit", JSON_Bool true) :: props
        else
          props
      in
      if Base.List.is_empty props then
        None
      else
        Some (JSON_Object props)
    in
    JSON_Object
      [
        ( "capabilities",
          Jprint.object_opt
            [
              ( "textDocumentSync",
                Some
                  (Jprint.object_opt
                     [
                       ("openClose", Some (JSON_Bool sync.want_openClose));
                       ("change", Some (print_textDocumentSyncKind sync.want_change));
                       ("willSave", Some (JSON_Bool sync.want_willSave));
                       ("willSaveWaitUntil", Some (JSON_Bool sync.want_willSaveWaitUntil));
                       ( "save",
                         Base.Option.map sync.want_didSave ~f:(fun save ->
                             JSON_Object [("includeText", JSON_Bool save.includeText)]) );
                     ]) );
              ("hoverProvider", Some (JSON_Bool cap.hoverProvider));
              ( "completionProvider",
                Base.Option.map cap.completionProvider ~f:CompletionOptionsFmt.to_json );
              ("selectionRangeProvider", Some (JSON_Bool cap.selectionRangeProvider));
              ( "signatureHelpProvider",
                Base.Option.map cap.signatureHelpProvider ~f:(fun shp ->
                    JSON_Object
                      [("triggerCharacters", Jprint.string_array shp.sighelp_triggerCharacters)]) );
              ("definitionProvider", Some (JSON_Bool cap.definitionProvider));
              ("typeDefinitionProvider", Some (JSON_Bool cap.typeDefinitionProvider));
              ("referencesProvider", Some (JSON_Bool cap.referencesProvider));
              ("documentHighlightProvider", Some (JSON_Bool cap.documentHighlightProvider));
              ("documentSymbolProvider", Some (JSON_Bool cap.documentSymbolProvider));
              ("workspaceSymbolProvider", Some (JSON_Bool cap.workspaceSymbolProvider));
              ( "codeActionProvider",
                Some
                  (match cap.codeActionProvider with
                  | CodeActionBool b -> JSON_Bool b
                  | CodeActionOptions { codeActionKinds } ->
                    JSON_Object
                      [
                        ( "codeActionKinds",
                          JSON_Array
                            (List.map
                               ~f:(fun k -> JSON_String (CodeActionKind.string_of_kind k))
                               codeActionKinds) );
                      ]) );
              ( "codeLensProvider",
                Base.Option.map cap.codeLensProvider ~f:(fun codelens ->
                    JSON_Object [("resolveProvider", JSON_Bool codelens.codelens_resolveProvider)])
              );
              ("documentFormattingProvider", Some (JSON_Bool cap.documentFormattingProvider));
              ( "documentRangeFormattingProvider",
                Some (JSON_Bool cap.documentRangeFormattingProvider) );
              ( "documentOnTypeFormattingProvider",
                Base.Option.map cap.documentOnTypeFormattingProvider ~f:(fun o ->
                    JSON_Object
                      [
                        ("firstTriggerCharacter", JSON_String o.firstTriggerCharacter);
                        ("moreTriggerCharacter", Jprint.string_array o.moreTriggerCharacter);
                      ]) );
              ("renameProvider", Some (JSON_Bool cap.renameProvider));
              ( "documentLinkProvider",
                Base.Option.map cap.documentLinkProvider ~f:(fun dlp ->
                    JSON_Object [("resolveProvider", JSON_Bool dlp.doclink_resolveProvider)]) );
              ( "executeCommandProvider",
                Base.Option.map cap.executeCommandProvider ~f:(fun p ->
                    let strs = p.commands |> Base.List.map ~f:(print_command_name ~key) in
                    JSON_Object [("commands", Jprint.string_array strs)]) );
              ("implementationProvider", Some (JSON_Bool cap.implementationProvider));
              ("typeCoverageProvider", Some (JSON_Bool cap.typeCoverageProvider));
              ("rageProvider", Some (JSON_Bool cap.rageProvider));
              ("experimental", experimental);
            ] );
      ])

module DidChangeWatchedFilesFmt = struct
  open Lsp.DidChangeWatchedFiles

  let params_of_json (json : Hh_json.json option) : params =
    let changes =
      Jget.array_exn json "changes"
      |> List.map ~f:(fun change ->
             let uri = Jget.string_exn change "uri" |> DocumentUri.of_string in
             let type_ = Jget.int_exn change "type" in
             let type_ =
               match fileChangeType_of_enum type_ with
               | Some type_ -> type_
               | None -> failwith (Printf.sprintf "Invalid file change type %d" type_)
             in
             { uri; type_ })
    in
    { changes }

  let json_of_register_options registerOptions =
    let open Hh_json in
    JSON_Object
      [
        ( "watchers",
          JSON_Array
            (List.map registerOptions.watchers ~f:(fun watcher ->
                 JSON_Object
                   [
                     ("globPattern", JSON_String watcher.globPattern);
                     ("kind", int_ 7);
                     (* all events: create, change, and delete *)
                   ])) );
      ]
end

(** capabilities *)
module RegisterCapabilityFmt = struct
  open Lsp.RegisterCapability

  let json_of_options (registerOptions : options) : Hh_json.json option =
    match registerOptions with
    | DidChangeConfiguration -> None
    | DidChangeWatchedFiles registerOptions ->
      Some (DidChangeWatchedFilesFmt.json_of_register_options registerOptions)

  let json_of_params (params : params) : Hh_json.json =
    let open Hh_json in
    JSON_Object
      [
        ( "registrations",
          JSON_Array
            (List.map params.registrations ~f:(fun registration ->
                 Jprint.object_opt
                   [
                     ("id", Some (string_ registration.id));
                     ("method", Some (string_ registration.method_));
                     ("registerOptions", json_of_options registration.registerOptions);
                   ])) );
      ]
end

(************************************************************************)
(* error response                                                       *)
(************************************************************************)

let error_of_exn (e : exn) : Lsp.Error.t =
  Lsp.Error.(
    match e with
    | Error.LspException x -> x
    | FlowExitStatus.Exit_with code ->
      { code = Error.UnknownErrorCode; message = FlowExitStatus.to_string code; data = None }
    | _ -> { code = Error.UnknownErrorCode; message = Printexc.to_string e; data = None })

let print_error ?(include_error_stack_trace = true) (e : Error.t) (stack : string) : json =
  let open Hh_json in
  let entries =
    if include_error_stack_trace then
      let stack_json_property = ("stack", string_ stack) in
      (* We'd like to add a stack-trace. The only place we can fit it, that will *)
      (* be respected by vscode-jsonrpc, is inside the 'data' field. And we can  *)
      (* do that only if data is an object. We can synthesize one if needed.     *)
      let data =
        match e.Error.data with
        | None -> JSON_Object [stack_json_property]
        | Some (JSON_Object o) -> JSON_Object (stack_json_property :: o)
        | Some primitive -> primitive
      in
      [("data", data)]
    else
      []
  in
  let entries =
    ("code", int_ (Error.code_to_enum e.Error.code))
    :: ("message", string_ e.Error.message) :: entries
  in
  JSON_Object entries

let parse_error (error : json) : Error.t =
  let json = Some error in
  let code =
    Jget.int_exn json "code"
    |> Error.code_of_enum
    |> Base.Option.value ~default:Error.UnknownErrorCode
  in
  let message = Jget.string_exn json "message" in
  let data = Jget.val_opt json "data" in
  { Error.code; message; data }

(************************************************************************)
(* universal parser+printer                                             *)
(************************************************************************)

let request_name_to_string (request : lsp_request) : string =
  match request with
  | ShowMessageRequestRequest _ -> "window/showMessageRequest"
  | ShowStatusRequest _ -> "window/showStatus"
  | InitializeRequest _ -> "initialize"
  | RegisterCapabilityRequest _ -> "client/registerCapability"
  | ShutdownRequest -> "shutdown"
  | CodeLensResolveRequest _ -> "codeLens/resolve"
  | HoverRequest _ -> "textDocument/hover"
  | CodeActionRequest _ -> "textDocument/codeAction"
  | CompletionRequest _ -> "textDocument/completion"
  | CompletionItemResolveRequest _ -> "completionItem/resolve"
  | ConfigurationRequest _ -> "workspace/configuration"
  | SelectionRangeRequest _ -> "textDocument/selectionRange"
  | SignatureHelpRequest _ -> "textDocument/signatureHelp"
  | DefinitionRequest _ -> "textDocument/definition"
  | TypeDefinitionRequest _ -> "textDocument/typeDefinition"
  | WorkspaceSymbolRequest _ -> "workspace/symbol"
  | DocumentSymbolRequest _ -> "textDocument/documentSymbol"
  | FindReferencesRequest _ -> "textDocument/references"
  | DocumentHighlightRequest _ -> "textDocument/documentHighlight"
  | TypeCoverageRequest _ -> "textDocument/typeCoverage"
  | DocumentFormattingRequest _ -> "textDocument/formatting"
  | DocumentRangeFormattingRequest _ -> "textDocument/rangeFormatting"
  | DocumentOnTypeFormattingRequest _ -> "textDocument/onTypeFormatting"
  | RageRequest -> "telemetry/rage"
  | RenameRequest _ -> "textDocument/rename"
  | DocumentCodeLensRequest _ -> "textDocument/codeLens"
  | ExecuteCommandRequest _ -> "workspace/executeCommand"
  | UnknownRequest (method_, _params) -> method_

let result_name_to_string (result : lsp_result) : string =
  match result with
  | ShowMessageRequestResult _ -> "window/showMessageRequest"
  | ShowStatusResult _ -> "window/showStatus"
  | InitializeResult _ -> "initialize"
  | ShutdownResult -> "shutdown"
  | CodeLensResolveResult _ -> "codeLens/resolve"
  | HoverResult _ -> "textDocument/hover"
  | CodeActionResult _ -> "textDocument/codeAction"
  | CompletionResult _ -> "textDocument/completion"
  | CompletionItemResolveResult _ -> "completionItem/resolve"
  | ConfigurationResult _ -> "workspace/configuration"
  | SelectionRangeResult _ -> "textDocument/selectionRange"
  | SignatureHelpResult _ -> "textDocument/signatureHelp"
  | DefinitionResult _ -> "textDocument/definition"
  | TypeDefinitionResult _ -> "textDocument/typeDefinition"
  | WorkspaceSymbolResult _ -> "workspace/symbol"
  | DocumentSymbolResult _ -> "textDocument/documentSymbol"
  | FindReferencesResult _ -> "textDocument/references"
  | GoToImplementationResult _ -> "textDocument/implementation"
  | DocumentHighlightResult _ -> "textDocument/documentHighlight"
  | TypeCoverageResult _ -> "textDocument/typeCoverage"
  | DocumentFormattingResult _ -> "textDocument/formatting"
  | DocumentRangeFormattingResult _ -> "textDocument/rangeFormatting"
  | DocumentOnTypeFormattingResult _ -> "textDocument/onTypeFormatting"
  | RageResult _ -> "telemetry/rage"
  | RenameResult _ -> "textDocument/rename"
  | DocumentCodeLensResult _ -> "textDocument/codeLens"
  | ExecuteCommandResult _ -> "workspace/executeCommand"
  | RegisterCapabilityResult -> "client/registerCapability"
  | ErrorResult (e, _stack) -> "ERROR/" ^ e.Error.message

let notification_name_to_string (notification : lsp_notification) : string =
  match notification with
  | ExitNotification -> "exit"
  | CancelRequestNotification _ -> "$/cancelRequest"
  | PublishDiagnosticsNotification _ -> "textDocument/publishDiagnostics"
  | DidOpenNotification _ -> "textDocument/didOpen"
  | DidCloseNotification _ -> "textDocument/didClose"
  | DidSaveNotification _ -> "textDocument/didSave"
  | DidChangeNotification _ -> "textDocument/didChange"
  | DidChangeConfigurationNotification _ -> "workspace/didChangeConfiguration"
  | DidChangeWatchedFilesNotification _ -> "workspace/didChangeWatchedFiles"
  | TelemetryNotification _ -> "telemetry/event"
  | LogMessageNotification _ -> "window/logMessage"
  | ShowMessageNotification _ -> "window/showMessage"
  | ConnectionStatusNotification _ -> "telemetry/connectionStatus"
  | InitializedNotification -> "initialized"
  | SetTraceNotification -> "$/setTraceNotification"
  | LogTraceNotification -> "$/logTraceNotification"
  | UnknownNotification (method_, _params) -> method_

let message_name_to_string (message : lsp_message) : string =
  match message with
  | RequestMessage (_, r) -> request_name_to_string r
  | NotificationMessage n -> notification_name_to_string n
  | ResponseMessage (_, r) -> result_name_to_string r

let denorm_message_to_string (message : lsp_message) : string =
  match message with
  | RequestMessage (id, r) ->
    Printf.sprintf "request %s %s" (id_to_string id) (request_name_to_string r)
  | NotificationMessage n -> Printf.sprintf "notification %s" (notification_name_to_string n)
  | ResponseMessage (id, ErrorResult (e, _stack)) ->
    Printf.sprintf "error %s %s" (id_to_string id) e.Error.message
  | ResponseMessage (id, r) ->
    Printf.sprintf "result %s %s" (id_to_string id) (result_name_to_string r)

let parse_lsp_request (method_ : string) (params : json option) : lsp_request =
  match method_ with
  | "initialize" -> InitializeRequest (parse_initialize params)
  | "shutdown" -> ShutdownRequest
  | "codeLens/resolve" -> CodeLensResolveRequest (parse_codeLensResolve params)
  | "textDocument/hover" -> HoverRequest (parse_hover params)
  | "textDocument/codeAction" -> CodeActionRequest (parse_codeActionRequest params)
  | "textDocument/completion" -> CompletionRequest (parse_completion params)
  | "textDocument/definition" -> DefinitionRequest (parse_definition params)
  | "workspace/symbol" -> WorkspaceSymbolRequest (parse_workspaceSymbol params)
  | "textDocument/documentSymbol" -> DocumentSymbolRequest (parse_documentSymbol params)
  | "textDocument/references" -> FindReferencesRequest (parse_findReferences params)
  | "textDocument/rename" -> RenameRequest (parse_documentRename params)
  | "textDocument/documentHighlight" -> DocumentHighlightRequest (parse_documentHighlight params)
  | "textDocument/typeCoverage" -> TypeCoverageRequest (parse_typeCoverage params)
  | "textDocument/formatting" -> DocumentFormattingRequest (parse_documentFormatting params)
  | "textDocument/rangeFormatting" ->
    DocumentRangeFormattingRequest (parse_documentRangeFormatting params)
  | "textDocument/onTypeFormatting" ->
    DocumentOnTypeFormattingRequest (parse_documentOnTypeFormatting params)
  | "textDocument/codeLens" -> DocumentCodeLensRequest (parse_documentCodeLens params)
  | "textDocument/selectionRange" -> SelectionRangeRequest (SelectionRangeFmt.params_of_json params)
  | "textDocument/signatureHelp" -> SignatureHelpRequest (SignatureHelpFmt.of_json params)
  | "telemetry/rage" -> RageRequest
  | "workspace/executeCommand" -> ExecuteCommandRequest (parse_executeCommand params)
  | "workspace/configuration" -> ConfigurationRequest (ConfigurationFmt.params_of_json params)
  | "completionItem/resolve"
  | "window/showMessageRequest"
  | "window/showStatus"
  | _ ->
    UnknownRequest (method_, params)

let parse_lsp_notification (method_ : string) (params : json option) : lsp_notification =
  match method_ with
  | "$/cancelRequest" -> CancelRequestNotification (parse_cancelRequest params)
  | "$/setTraceNotification" -> SetTraceNotification
  | "$/logTraceNotification" -> LogTraceNotification
  | "initialized" -> InitializedNotification
  | "exit" -> ExitNotification
  | "textDocument/didOpen" -> DidOpenNotification (parse_didOpen params)
  | "textDocument/didClose" -> DidCloseNotification (parse_didClose params)
  | "textDocument/didSave" -> DidSaveNotification (parse_didSave params)
  | "textDocument/didChange" -> DidChangeNotification (parse_didChange params)
  | "workspace/didChangeConfiguration" ->
    DidChangeConfigurationNotification (DidChangeConfigurationFmt.params_of_json params)
  | "workspace/didChangeWatchedFiles" ->
    DidChangeWatchedFilesNotification (DidChangeWatchedFilesFmt.params_of_json params)
  | "textDocument/publishDiagnostics"
  | "window/logMessage"
  | "window/showMessage"
  | "telemetry/connectionStatus"
  | _ ->
    UnknownNotification (method_, params)

let parse_lsp_result (request : lsp_request) (result : json) : lsp_result =
  let method_ = request_name_to_string request in
  match request with
  | ShowMessageRequestRequest _ ->
    ShowMessageRequestResult (parse_result_showMessageRequest (Some result))
  | ShowStatusRequest _ -> ShowStatusResult (parse_result_showMessageRequest (Some result))
  | ConfigurationRequest _ -> ConfigurationResult (ConfigurationFmt.result_of_json (Some result))
  | RegisterCapabilityRequest _ -> RegisterCapabilityResult
  | InitializeRequest _
  | ShutdownRequest
  | CodeLensResolveRequest _
  | HoverRequest _
  | CodeActionRequest _
  | CompletionRequest _
  | CompletionItemResolveRequest _
  | SelectionRangeRequest _
  | SignatureHelpRequest _
  | DefinitionRequest _
  | TypeDefinitionRequest _
  | WorkspaceSymbolRequest _
  | DocumentSymbolRequest _
  | FindReferencesRequest _
  | DocumentHighlightRequest _
  | TypeCoverageRequest _
  | DocumentFormattingRequest _
  | DocumentRangeFormattingRequest _
  | DocumentOnTypeFormattingRequest _
  | RageRequest
  | RenameRequest _
  | DocumentCodeLensRequest _
  | ExecuteCommandRequest _
  | UnknownRequest _ ->
    raise
      (Error.LspException
         {
           Error.code = Error.ParseError;
           message = "Don't know how to parse LSP response " ^ method_;
           data = None;
         })

(* parse_lsp: non-jsonrpc inputs - will raise an exception                    *)
(* requests and notifications - will raise an exception if they're malformed, *)
(*   otherwise return Some                                                    *)
(* responses - will raise an exception if they're malformed, will return None *)
(*   if they're absent from the "outstanding" map, otherwise return Some.     *)
let parse_lsp (json : json) (outstanding : lsp_id -> lsp_request) : lsp_message =
  let json = Some json in
  let id = Jget.val_opt json "id" |> parse_id_opt in
  let method_opt = Jget.string_opt json "method" in
  let params = Jget.val_opt json "params" in
  let result = Jget.val_opt json "result" in
  let error = Jget.val_opt json "error" in
  match (id, method_opt, result, error) with
  | (None, Some method_, _, _) -> NotificationMessage (parse_lsp_notification method_ params)
  | (Some id, Some method_, _, _) -> RequestMessage (id, parse_lsp_request method_ params)
  | (Some id, _, Some result, _) ->
    let request = outstanding id in
    ResponseMessage (id, parse_lsp_result request result)
  | (Some id, _, _, Some error) -> ResponseMessage (id, ErrorResult (parse_error error, ""))
  | (_, _, _, _) ->
    raise
      (Error.LspException { Error.code = Error.ParseError; message = "Not JsonRPC"; data = None })

let print_lsp_request (id : lsp_id) (request : lsp_request) : json =
  let method_ = request_name_to_string request in
  let params =
    match request with
    | ShowMessageRequestRequest r -> print_showMessageRequest r
    | ShowStatusRequest r -> print_showStatus r
    | RegisterCapabilityRequest r -> RegisterCapabilityFmt.json_of_params r
    | ConfigurationRequest r -> ConfigurationFmt.json_of_params r
    | InitializeRequest _
    | ShutdownRequest
    | HoverRequest _
    | CodeActionRequest _
    | CodeLensResolveRequest _
    | CompletionRequest _
    | CompletionItemResolveRequest _
    | SelectionRangeRequest _
    | SignatureHelpRequest _
    | DefinitionRequest _
    | TypeDefinitionRequest _
    | WorkspaceSymbolRequest _
    | DocumentSymbolRequest _
    | FindReferencesRequest _
    | DocumentHighlightRequest _
    | TypeCoverageRequest _
    | DocumentFormattingRequest _
    | DocumentRangeFormattingRequest _
    | DocumentOnTypeFormattingRequest _
    | RageRequest
    | RenameRequest _
    | DocumentCodeLensRequest _
    | ExecuteCommandRequest _
    | UnknownRequest _ ->
      failwith ("Don't know how to print request " ^ method_)
  in
  JSON_Object
    [
      ("jsonrpc", JSON_String "2.0");
      ("id", print_id id);
      ("method", JSON_String method_);
      ("params", params);
    ]

let print_lsp_response ?include_error_stack_trace ~key (id : lsp_id) (result : lsp_result) : json =
  let method_ = result_name_to_string result in
  let json =
    match result with
    | InitializeResult r -> print_initialize ~key r
    | ShutdownResult -> print_shutdown ()
    | CodeLensResolveResult r -> print_codeLensResolve ~key r
    | HoverResult r -> print_hover r
    | CodeActionResult r -> print_codeActionResult ~key r
    | CompletionResult r -> print_completion ~key r
    | ConfigurationResult r -> ConfigurationFmt.json_of_result r
    | DefinitionResult r -> print_definition r
    | TypeDefinitionResult r -> print_definition r
    | WorkspaceSymbolResult r -> print_workspaceSymbol r
    | DocumentSymbolResult r -> print_documentSymbol r
    | FindReferencesResult r -> print_Locations r
    | GoToImplementationResult r -> print_Locations r
    | DocumentHighlightResult r -> print_documentHighlight r
    | TypeCoverageResult r -> print_typeCoverage r
    | DocumentFormattingResult r -> print_documentFormatting r
    | DocumentRangeFormattingResult r -> print_documentRangeFormatting r
    | DocumentOnTypeFormattingResult r -> print_documentOnTypeFormatting r
    | RageResult r -> print_rage r
    | RenameResult r -> print_documentRename r
    | DocumentCodeLensResult r -> print_documentCodeLens ~key r
    | ExecuteCommandResult r -> print_executeCommand r
    | SelectionRangeResult r -> SelectionRangeFmt.json_of_result r
    | SignatureHelpResult r -> SignatureHelpFmt.to_json r
    | ShowMessageRequestResult _
    | ShowStatusResult _
    | CompletionItemResolveResult _
    | RegisterCapabilityResult ->
      failwith ("Don't know how to print result " ^ method_)
    | ErrorResult (e, stack) -> print_error ?include_error_stack_trace e stack
  in
  match result with
  | ErrorResult _ ->
    JSON_Object [("jsonrpc", JSON_String "2.0"); ("id", print_id id); ("error", json)]
  | _ -> JSON_Object [("jsonrpc", JSON_String "2.0"); ("id", print_id id); ("result", json)]

let print_lsp_notification (notification : lsp_notification) : json =
  let method_ = notification_name_to_string notification in
  let params =
    match notification with
    | CancelRequestNotification r -> print_cancelRequest r
    | PublishDiagnosticsNotification r -> print_diagnostics r
    | TelemetryNotification r -> print_logMessage r.LogMessage.type_ r.LogMessage.message
    | LogMessageNotification r -> print_logMessage r.LogMessage.type_ r.LogMessage.message
    | ShowMessageNotification r -> print_showMessage r.ShowMessage.type_ r.ShowMessage.message
    | ConnectionStatusNotification r -> print_connectionStatus r
    | ExitNotification
    | InitializedNotification
    | SetTraceNotification
    | LogTraceNotification
    | DidOpenNotification _
    | DidCloseNotification _
    | DidSaveNotification _
    | DidChangeNotification _
    | DidChangeConfigurationNotification _
    | DidChangeWatchedFilesNotification _
    | UnknownNotification _ ->
      failwith ("Don't know how to print notification " ^ method_)
  in
  JSON_Object [("jsonrpc", JSON_String "2.0"); ("method", JSON_String method_); ("params", params)]

let print_lsp ?include_error_stack_trace ~key (message : lsp_message) : json =
  match message with
  | RequestMessage (id, request) -> print_lsp_request id request
  | ResponseMessage (id, result) -> print_lsp_response ?include_error_stack_trace ~key id result
  | NotificationMessage notification -> print_lsp_notification notification
