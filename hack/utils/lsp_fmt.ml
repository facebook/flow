(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

open Hh_core
open Lsp
open Hh_json
open Hh_json_helpers


(************************************************************************)
(** Miscellaneous LSP structures                                       **)
(************************************************************************)

let parse_id (json: json) : lsp_id =
  match json with
  | JSON_Number s ->
    begin try NumberId (int_of_string s)
    with Failure _ -> raise (Error.Parse ("float ids not allowed: " ^ s)) end
  | JSON_String s ->
    StringId s
  | _ ->
    raise (Error.Parse ("not an id: " ^ (Hh_json.json_to_string json)))

let parse_id_opt (json: json option) : lsp_id option =
  Option.map json ~f:parse_id

let print_id (id: lsp_id) : json =
  match id with
  | NumberId n -> JSON_Number (string_of_int n)
  | StringId s -> JSON_String s

let id_to_string (id: lsp_id) : string =
  match id with
  | NumberId n -> string_of_int n
  | StringId s -> Printf.sprintf "\"%s\"" s

let parse_position (json: json option) : position =
  {
    line = Jget.int_exn json "line";
    character = Jget.int_exn json "character";
  }

let print_position (position: position) : json =
  JSON_Object [
    "line", position.line |> int_;
    "character", position.character |> int_;
  ]

let print_range (range: range) : json =
  JSON_Object [
    "start", print_position range.start;
    "end", print_position range.end_;
  ]

let print_location (location: Location.t) : json =
  let open Location in
  JSON_Object [
    "uri", JSON_String location.uri;
    "range", print_range location.range;
  ]

let print_definition_location (definition_location: DefinitionLocation.t) : json =
  let open DefinitionLocation in
  let location = definition_location.location in
  Jprint.object_opt [
    "uri", Some (JSON_String location.Location.uri);
    "range", Some (print_range location.Location.range);
    "title", Option.map definition_location.title ~f:string_;
  ]

let parse_range_exn (json: json option) : range =
  {
    start = Jget.obj_exn json "start" |> parse_position;
    end_ = Jget.obj_exn json "end" |> parse_position;
  }

let parse_range_opt (json: json option) : range option =
  if json = None then None
  else Some (parse_range_exn json)

let parse_textDocumentIdentifier (json: json option)
  : TextDocumentIdentifier.t =
  let open TextDocumentIdentifier in
  {
    uri = Jget.string_exn json "uri";
  }

let parse_versionedTextDocumentIdentifier (json: json option)
  : VersionedTextDocumentIdentifier.t =
  let open VersionedTextDocumentIdentifier in
  {
    uri = Jget.string_exn json "uri";
    version = Jget.int_d json "version" 0;
  }

let parse_textDocumentItem (json: json option) : TextDocumentItem.t =
  let open TextDocumentItem in
  {
    uri = Jget.string_exn json "uri";
    languageId = Jget.string_d json "languageId" "";
    version = Jget.int_d json "version" 0;
    text = Jget.string_exn json "text";
  }

let print_textDocumentItem (item: TextDocumentItem.t) : json =
  let open TextDocumentItem in
  JSON_Object [
    "uri", JSON_String item.uri;
    "languageId", JSON_String item.languageId;
    "version", JSON_Number (string_of_int item.version);
    "text", JSON_String item.text;
  ]

let print_markedItem (item: markedString) : json =
  match item with
  | MarkedString s -> JSON_String s
  | MarkedCode (language, value) -> JSON_Object
      [
        "language", JSON_String language;
        "value", JSON_String value;
      ]

let parse_textDocumentPositionParams (params: json option)
  : TextDocumentPositionParams.t =
  let open TextDocumentPositionParams in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
    position = Jget.obj_exn params "position" |> parse_position;
  }

let parse_textEdit (params: json option) : TextEdit.t option =
  match params with
  | None -> None
  | _ ->
    let open TextEdit in
    Some {
      range = Jget.obj_exn params "range" |> parse_range_exn;
      newText = Jget.string_exn params "newText";
    }

let print_textEdit (edit: TextEdit.t) : json =
  let open TextEdit in
  JSON_Object [
    "range", print_range edit.range;
    "newText", JSON_String edit.newText;
  ]

let print_command (command: Command.t) : json =
  let open Command in
  JSON_Object [
    "title", JSON_String command.title;
    "command", JSON_String command.command;
    "arguments", JSON_Array command.arguments;
  ]

let parse_command (json: json option) : Command.t =
  let open Command in
  {
    title = Jget.string_d json "title" "";
    command = Jget.string_d json "command" "";
    arguments = Jget.array_d json "arguments" ~default:[] |> List.filter_opt;
  }

let parse_formattingOptions (json: json option)
  : DocumentFormatting.formattingOptions =
  { DocumentFormatting.
    tabSize = Jget.int_d json "tabSize" 2;
    insertSpaces = Jget.bool_d json "insertSpaces" true;
  }

let print_symbolInformation (info: SymbolInformation.t) : json =
  let open SymbolInformation in
  let print_symbol_kind = function
    | File -> int_ 1
    | Module -> int_ 2
    | Namespace -> int_ 3
    | Package -> int_ 4
    | Class -> int_ 5
    | Method -> int_ 6
    | Property -> int_ 7
    | Field -> int_ 8
    | Constructor -> int_ 9
    | Enum -> int_ 10
    | Interface -> int_ 11
    | Function -> int_ 12
    | Variable -> int_ 13
    | Constant -> int_ 14
    | String -> int_ 15
    | Number -> int_ 16
    | Boolean -> int_ 17
    | Array -> int_ 18
  in
  Jprint.object_opt [
    "name", Some (JSON_String info.name);
    "kind", Some (print_symbol_kind info.kind);
    "location", Some (print_location info.location);
    "containerName", Option.map info.containerName string_;
  ]

let print_messageType (type_: MessageType.t) : json =
  let open MessageType in
  match type_ with
  | ErrorMessage -> int_ 1
  | WarningMessage -> int_ 2
  | InfoMessage -> int_ 3
  | LogMessage -> int_ 4


(************************************************************************)
(** shutdown request                                                   **)
(************************************************************************)

let print_shutdown () : json =
  JSON_Null


(************************************************************************)
(** $/cancelRequest notification                                       **)
(************************************************************************)

let parse_cancelRequest (params: json option) : CancelRequest.params =
  let open CancelRequest in
  {
    id = Jget.val_exn params "id" |> parse_id
  }

let print_cancelRequest (p: CancelRequest.params) : json =
  let open CancelRequest in
  JSON_Object [
    "id", print_id p.id
  ]

(************************************************************************)
(** rage request                                                       **)
(************************************************************************)

let print_rage (r: Rage.result) : json =
  let open Rage in
  let print_item (item: rageItem) : json =
    JSON_Object [
      "data", JSON_String item.data;
      "title", match item.title with None -> JSON_Null | Some s -> JSON_String s;
    ] in
  JSON_Array (List.map r ~f:print_item)


(************************************************************************)
(** textDocument/didOpen notification                                  **)
(************************************************************************)

let parse_didOpen (params: json option) : DidOpen.params =
  let open DidOpen in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentItem;
  }

let print_didOpen (params: DidOpen.params) : json =
  let open DidOpen in
  JSON_Object [
    "textDocument", params.textDocument |> print_textDocumentItem;
  ]

(************************************************************************)
(** textDocument/didClose notification                                 **)
(************************************************************************)

let parse_didClose (params: json option) : DidClose.params =
  let open DidClose in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
  }


(************************************************************************)
(** textDocument/didSave notification                                  **)
(************************************************************************)

let parse_didSave (params: json option) : DidSave.params =
  let open DidSave in
  {
    textDocument = Jget.obj_exn params "textDocument" |> parse_textDocumentIdentifier;
    text = Jget.string_opt params "text";
  }



(************************************************************************)
(** textDocument/didChange notification                                **)
(************************************************************************)

let parse_didChange (params: json option) : DidChange.params =
  let open DidChange in
  let parse_textDocumentContentChangeEvent json =
    {
      range = Jget.obj_opt json "range" |> parse_range_opt;
      rangeLength = Jget.int_opt json "rangeLength";
      text = Jget.string_exn json "text";
    }
  in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_versionedTextDocumentIdentifier;
    contentChanges = Jget.array_d params "contentChanges" ~default:[]
                     |> List.map ~f:parse_textDocumentContentChangeEvent;
  }



(************************************************************************)
(** textDocument/signatureHelp notification                            **)
(************************************************************************)

let parse_signatureHelp (params: json option) : SignatureHelp.params =
  parse_textDocumentPositionParams params

let print_signatureHelp (r: SignatureHelp.result) : json =
  let open SignatureHelp in
  let print_parInfo parInfo =
    Jprint.object_opt [
      "label", Some (Hh_json.JSON_String parInfo.parinfo_label);
      "documentation", Option.map ~f:Hh_json.string_ parInfo.parinfo_documentation;
    ]
  in
  let print_sigInfo sigInfo =
    Jprint.object_opt [
      "label", Some (Hh_json.JSON_String sigInfo.siginfo_label);
      "documentation", Option.map ~f:Hh_json.string_ sigInfo.siginfo_documentation;
      "parameters", Some (Hh_json.JSON_Array (List.map ~f:print_parInfo sigInfo.parameters))
    ]
  in
  match r with
  | None -> Hh_json.JSON_Null
  | Some r ->
    Hh_json.JSON_Object [
      "signatures", Hh_json.JSON_Array (List.map ~f:print_sigInfo r.signatures);
      "activeSignature", Hh_json.int_ r.activeSignature;
      "activeParameter", Hh_json.int_ r.activeParameter;
    ]



(************************************************************************)
(** textDocument/rename Request                                        **)
(************************************************************************)

let parse_documentRename (params: json option) : Rename.params =
  let open Rename in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
    position = Jget.obj_exn params "position" |> parse_position;
    newName = Jget.string_exn params "newName";
  }

let print_documentRename (r: Rename.result) : json =
  let open WorkspaceEdit in
  let print_workspace_edit_changes (uri, text_edits) =
      uri, JSON_Array (List.map ~f:print_textEdit text_edits)
  in
  JSON_Object [
    "changes", JSON_Object (List.map (SMap.elements r.changes) ~f:print_workspace_edit_changes);
  ]



(************************************************************************)
(** textDocument/publishDiagnostics notification                       **)
(************************************************************************)

let print_diagnostics (r: PublishDiagnostics.params) : json =
  let open PublishDiagnostics in
  let print_diagnosticSeverity = function
    | PublishDiagnostics.Error -> int_ 1
    | PublishDiagnostics.Warning -> int_ 2
    | PublishDiagnostics.Information -> int_ 3
    | PublishDiagnostics.Hint -> int_ 4 in
  let print_diagnosticCode = function
    | IntCode i -> Some (int_ i)
    | StringCode s -> Some (string_ s)
    | NoCode -> None
  in
  let print_related (related: relatedLocation) : json =
    Hh_json.JSON_Object [
      "location", print_location related.relatedLocation;
      "message", string_ related.relatedMessage;
    ]
  in
  let print_diagnostic (diagnostic: diagnostic) : json =
    Jprint.object_opt [
      "range", Some (print_range diagnostic.range);
      "severity", Option.map diagnostic.severity print_diagnosticSeverity;
      "code", print_diagnosticCode diagnostic.code;
      "source", Option.map diagnostic.source string_;
      "message", Some (JSON_String diagnostic.message);
      "relatedInformation",
        Some (JSON_Array (List.map diagnostic.relatedInformation ~f:print_related));
      "relatedLocations", Some (JSON_Array (List.map diagnostic.relatedLocations ~f:print_related));
    ]
  in
  JSON_Object [
    "uri", JSON_String r.uri;
    "diagnostics", JSON_Array (List.map r.diagnostics ~f:print_diagnostic)
  ]


(************************************************************************)
(** window/logMessage notification                                     **)
(************************************************************************)

let print_logMessage (type_: MessageType.t) (message: string) : json =
  let open LogMessage in
  let r = { type_; message; } in
  JSON_Object [
    "type", print_messageType r.type_;
    "message", JSON_String r.message;
  ]


(************************************************************************)
(** window/showMessage notification                                    **)
(************************************************************************)

let print_showMessage (type_: MessageType.t) (message: string) : json =
  let open ShowMessage in
  let r = { type_; message; } in
  JSON_Object [
    "type", print_messageType r.type_;
    "message", JSON_String r.message;
  ]

(************************************************************************)
(** window/showMessage request                                         **)
(************************************************************************)

let print_showMessageRequest (r: ShowMessageRequest.showMessageRequestParams) : json =
  let print_action (action: ShowMessageRequest.messageActionItem) : json =
    JSON_Object [
      "title", JSON_String action.ShowMessageRequest.title;
    ]
  in
  Jprint.object_opt [
    "type", Some (print_messageType r.ShowMessageRequest.type_);
    "message", Some (JSON_String r.ShowMessageRequest.message);
    "actions", Some (JSON_Array (List.map r.ShowMessageRequest.actions ~f:print_action));
  ]

let parse_result_showMessageRequest (result: json option) : ShowMessageRequest.result =
  let open ShowMessageRequest in
  let title = Jget.string_opt result "title" in
  Option.map title ~f:(fun title -> { title; })


(************************************************************************)
(** window/showStatus request                                          **)
(************************************************************************)

let print_showStatus (r: ShowStatus.showStatusParams) : json =
  let print_action (action: ShowMessageRequest.messageActionItem) : json =
    JSON_Object [
      "title", JSON_String action.ShowMessageRequest.title;
    ]
  in
  let rr = r.ShowStatus.request in
  Jprint.object_opt [
    "type", Some (print_messageType rr.ShowMessageRequest.type_);
    "actions", Some (JSON_Array (List.map rr.ShowMessageRequest.actions ~f:print_action));
    "message", Some (JSON_String rr.ShowMessageRequest.message);
    "shortMessage", Option.map r.ShowStatus.shortMessage ~f:string_;
    "progress", Option.map r.ShowStatus.progress ~f:(fun progress -> Jprint.object_opt [
      "numerator", Some (int_ progress);
      "denominator", Option.map r.ShowStatus.total ~f:int_;
      ]);
  ]


(************************************************************************)
(** window/progress notification                                       **)
(************************************************************************)

let print_progress (id: int) (label: string option) : json =
  let r = { Progress.id; label; } in
  JSON_Object [
    "id", r.Progress.id |> int_;
    "label", match r.Progress.label with None -> JSON_Null | Some s -> JSON_String s;
  ]


(************************************************************************)
(** window/actionRequired notification                                 **)
(************************************************************************)

let print_actionRequired (id: int) (label: string option) : json =
  let r = { ActionRequired.id; label; } in
  JSON_Object [
    "id", r.ActionRequired.id |> int_;
    "label", match r.ActionRequired.label with None -> JSON_Null | Some s -> JSON_String s;
  ]


(************************************************************************)
(** telemetry/connectionStatus notification                            **)
(************************************************************************)

let print_connectionStatus (p: ConnectionStatus.params) : json =
  let open ConnectionStatus in
  JSON_Object [
    "isConnected", JSON_Bool p.isConnected;
  ]


(************************************************************************)
(** textDocument/hover request                                         **)
(************************************************************************)

let parse_hover (params: json option) : Hover.params =
  parse_textDocumentPositionParams params

let print_hover (r: Hover.result) : json =
  let open Hover in
  match r with
  | None ->
    JSON_Null
  | Some r ->
    Jprint.object_opt [
      "contents", Some (JSON_Array
          (List.map r.Hover.contents ~f:print_markedItem));
      "range", Option.map r.range ~f:print_range;
    ]


(************************************************************************)
(** textDocument/definition request                                    **)
(************************************************************************)

let parse_definition (params: json option) : Definition.params =
  parse_textDocumentPositionParams params

let print_definition (r: Definition.result) : json =
  JSON_Array (List.map r ~f:print_definition_location)


(************************************************************************)
(** completionItem/resolve request                                     **)
(************************************************************************)

let parse_completionItem (params: json option) : CompletionItemResolve.params =
  let open Completion in
  let textEdits =
    (Jget.obj_opt params "textEdit") :: (Jget.array_d params "additionalTextEdits" ~default:[])
    |> List.filter_map ~f:parse_textEdit
  in
  let command = match Jget.obj_opt params "command" with
    | None -> None
    | c -> Some (parse_command c)
  in
  {
    label = Jget.string_exn params "label";
    kind = Option.bind (Jget.int_opt params "kind") completionItemKind_of_int_opt;
    detail = Jget.string_opt params "detail";
    inlineDetail = Jget.string_opt params "inlineDetail";
    itemType = Jget.string_opt params "itemType";
    documentation = Jget.string_opt params "documentation";
    sortText = Jget.string_opt params "sortText";
    filterText = Jget.string_opt params "filterText";
    insertText = Jget.string_opt params "insertText";
    insertTextFormat = Option.bind (Jget.int_opt params "insertTextFormat") insertFormat_of_int_opt;
    textEdits;
    command;
    data = Jget.obj_opt params "data"
  }


let print_completionItem (item: Completion.completionItem) : json =
  let open Completion in
  Jprint.object_opt [
    "label", Some (JSON_String item.label);
    "kind", Option.map item.kind (fun x -> int_ @@ int_of_completionItemKind x);
    "detail", Option.map item.detail string_;
    "inlineDetail", Option.map item.inlineDetail string_;
    "itemType", Option.map item.itemType string_;
    "documentation", Option.map item.documentation string_;
    "sortText", Option.map item.sortText string_;
    "filterText", Option.map item.filterText string_;
    "insertText", Option.map item.insertText string_;
    "insertTextFormat", Option.map item.insertTextFormat (fun x -> int_ @@ int_of_insertFormat x);
    "textEdit", Option.map (List.hd item.textEdits) print_textEdit;
    "additionalTextEdit", (match (List.tl item.textEdits) with
      | None | Some [] -> None
      | Some l -> Some (JSON_Array (List.map l ~f:print_textEdit)));
    "command", Option.map item.command print_command;
    "data", item.data;
  ]


(************************************************************************)
(** textDocument/completion request                                    **)
(************************************************************************)

let parse_completion (params: json option) : Completion.params =
  let open Lsp.Completion in
  let context = Jget.obj_opt params "context" in
  {
    loc = parse_textDocumentPositionParams params;
    context = match context with
    | Some _ ->
      Some {
        triggerKind = (match Jget.int_exn context "triggerKind" with
        | 1 -> Invoked
        | 2 -> TriggerCharacter
        | 3 -> TriggerForIncompleteCompletions
        | x -> failwith ("Unsupported trigger kind: "^(string_of_int x))
        );
      }
    | None -> None
  }

let print_completion (r: Completion.result) : json =
  let open Completion in
  JSON_Object [
    "isIncomplete", JSON_Bool r.isIncomplete;
    "items", JSON_Array (List.map r.items ~f:print_completionItem);
  ]


(************************************************************************)
(** workspace/symbol request                                           **)
(************************************************************************)


let parse_workspaceSymbol (params: json option) : WorkspaceSymbol.params =
  let open WorkspaceSymbol in
  {
    query = Jget.string_exn params "query";
  }

let print_workspaceSymbol (r: WorkspaceSymbol.result) : json =
  JSON_Array (List.map r ~f:print_symbolInformation)


(************************************************************************)
(** textDocument/documentSymbol request                                **)
(************************************************************************)

let parse_documentSymbol (params: json option) : DocumentSymbol.params =
  let open DocumentSymbol in
  {
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
  }

let print_documentSymbol (r: DocumentSymbol.result) : json =
  JSON_Array (List.map r ~f:print_symbolInformation)


(************************************************************************)
(** textDocument/references request                                    **)
(************************************************************************)

let parse_findReferences (params: json option) : FindReferences.params =
  let context = Jget.obj_opt params "context" in
  { FindReferences.
    loc = parse_textDocumentPositionParams params;
    context =
      { FindReferences.
        includeDeclaration = Jget.bool_d context "includeDeclaration" true;
        includeIndirectReferences = Jget.bool_d context "includeIndirectReferences" false;
      }
  }

let print_findReferences (r: Location.t list) : json =
  JSON_Array (List.map r ~f:print_location)


(************************************************************************)
(** textDocument/documentHighlight request                            **)
(************************************************************************)

let parse_documentHighlight (params: json option)
  : DocumentHighlight.params =
  parse_textDocumentPositionParams params

let print_documentHighlight (r: DocumentHighlight.result) : json =
  let open DocumentHighlight in
  let print_highlightKind kind = match kind with
    | Text -> int_ 1
    | Read -> int_ 2
    | Write -> int_ 3
  in
  let print_highlight highlight =
    Jprint.object_opt [
      "range", Some (print_range highlight.range);
      "kind", Option.map highlight.kind ~f:print_highlightKind
    ]
  in
  JSON_Array (List.map r ~f:print_highlight)


(************************************************************************)
(** textDocument/typeCoverage request                                  **)
(************************************************************************)

let parse_typeCoverage (params: json option)
  : TypeCoverage.params =
  { TypeCoverage.
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
  }

let print_typeCoverage (r: TypeCoverage.result) : json =
  let open TypeCoverage in
  let print_uncov (uncov: uncoveredRange) : json =
    Jprint.object_opt [
      "range", Some (print_range uncov.range);
      "message", Option.map uncov.message ~f:string_;
    ]
  in
  JSON_Object [
    "coveredPercent", int_ r.coveredPercent;
    "uncoveredRanges", JSON_Array (List.map r.uncoveredRanges ~f:print_uncov);
    "defaultMessage", JSON_String r.defaultMessage;
  ]

(************************************************************************)
(** workspace/toggleTypeCoverage request                                      **)
(************************************************************************)
let parse_toggleTypeCoverage (params: json option)
  : ToggleTypeCoverage.params =
  { ToggleTypeCoverage.
    toggle = Jget.bool_d params "toggle" ~default:false
  }

(************************************************************************)
(** textDocument/formatting request                                    **)
(************************************************************************)

let parse_documentFormatting (params: json option)
  : DocumentFormatting.params =
  { DocumentFormatting.
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentFormatting (r: DocumentFormatting.result)
  : json =
  JSON_Array (List.map r ~f:print_textEdit)


(************************************************************************)
(** textDocument/rangeFormatting request                               **)
(************************************************************************)

let parse_documentRangeFormatting (params: json option)
  : DocumentRangeFormatting.params =
  { DocumentRangeFormatting.
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
    range = Jget.obj_exn params "range" |> parse_range_exn;
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentRangeFormatting (r: DocumentRangeFormatting.result)
  : json =
  JSON_Array (List.map r ~f:print_textEdit)


(************************************************************************)
(** textDocument/onTypeFormatting request                              **)
(************************************************************************)

let parse_documentOnTypeFormatting (params: json option)
  : DocumentOnTypeFormatting.params =
  { DocumentOnTypeFormatting.
    textDocument = Jget.obj_exn params "textDocument"
                   |> parse_textDocumentIdentifier;
    position = Jget.obj_exn params "position" |> parse_position;
    ch = Jget.string_exn params "ch";
    options = Jget.obj_opt params "options" |> parse_formattingOptions;
  }

let print_documentOnTypeFormatting (r: DocumentOnTypeFormatting.result)
  : json =
  JSON_Array (List.map r ~f:print_textEdit)


(************************************************************************)
(** initialize request                                                 **)
(************************************************************************)

let parse_initialize (params: json option) : Initialize.params =
  let open Initialize in
  let rec parse_initialize json =
    {
      processId = Jget.int_opt json "processId";
      rootPath = Jget.string_opt json "rootPath";
      rootUri = Jget.string_opt json "rootUri";
      initializationOptions = Jget.obj_opt json "initializationOptions"
                              |> parse_initializationOptions;
      client_capabilities = Jget.obj_opt json "capabilities"
                            |> parse_capabilities;
      trace = Jget.string_opt json "trace" |> parse_trace;
    }
  and parse_trace (s : string option) : trace = match s with
    | Some "messages" -> Messages
    | Some "verbose" -> Verbose
    | _ -> Off
  and parse_initializationOptions json =
    {
      useTextEditAutocomplete = Jget.bool_d json "useTextEditAutocomplete" ~default:false;
      liveSyntaxErrors = Jget.bool_d json "liveSyntaxErrors" ~default:true;
    }
  and parse_capabilities json =
    {
      workspace = Jget.obj_opt json "workspace" |> parse_workspace;
      textDocument = Jget.obj_opt json "textDocument" |> parse_textDocument;
      window = Jget.obj_opt json "window" |> parse_window;
      telemetry = Jget.obj_opt json "telemetry" |> parse_telemetry;
    }
  and parse_workspace json =
    {
      applyEdit = Jget.bool_d json "applyEdit" ~default:false;
      workspaceEdit = Jget.obj_opt json "workspaceEdit"
                       |> parse_workspaceEdit;
    }
  and parse_workspaceEdit json =
    {
      documentChanges = Jget.bool_d json "documentChanges" ~default:false;
    }
  and parse_textDocument json =
    {
      synchronization =
        Jget.obj_opt json "synchronization" |> parse_synchronization;
      completion = Jget.obj_opt json "completion" |> parse_completion;
    }
  and parse_synchronization json =
    {
      can_willSave = Jget.bool_d json "willSave" ~default:false;
      can_willSaveWaitUntil =
        Jget.bool_d json "willSaveWaitUntil" ~default:false;
      can_didSave = Jget.bool_d json "didSave" ~default:false;
    }
  and parse_completion json =
    { completionItem =
        Jget.obj_opt json "completionItem" |> parse_completionItem;
    }
  and parse_completionItem json =
    { snippetSupport = Jget.bool_d json "snippetSupport" ~default:false;
    }
  and parse_window json =
    {
      status = Jget.obj_opt json "status" |> Option.is_some;
      progress = Jget.obj_opt json "progress" |> Option.is_some;
      actionRequired = Jget.obj_opt json "actionRequired" |> Option.is_some;
    }
  and parse_telemetry json =
    {
      connectionStatus = Jget.obj_opt json "connectionStatus" |> Option.is_some;
    }
  in
  parse_initialize params

let print_initializeError (r: Initialize.errorData) : json =
  let open Initialize in
  JSON_Object [
    "retry", JSON_Bool r.retry;
  ]

let print_initialize (r: Initialize.result) : json =
  let open Initialize in
  let print_textDocumentSyncKind = function
    | NoSync -> int_ 0
    | FullSync -> int_ 1
    | IncrementalSync -> int_ 2 in
  let cap = r.server_capabilities in
  let sync = cap.textDocumentSync
  in
  JSON_Object [
    "capabilities", Jprint.object_opt [
      "textDocumentSync", Some (Jprint.object_opt [
        "openClose", Some (JSON_Bool sync.want_openClose);
        "change", Some (print_textDocumentSyncKind sync.want_change);
        "willSave", Some (JSON_Bool sync.want_willSave);
        "willSaveWaitUntil", Some (JSON_Bool sync.want_willSaveWaitUntil);
        "save", Option.map sync.want_didSave ~f:(fun save -> JSON_Object [
          "includeText", JSON_Bool save.includeText;
        ]);
      ]);
      "hoverProvider", Some (JSON_Bool cap.hoverProvider);
      "completionProvider", Option.map cap.completionProvider ~f:(fun comp -> JSON_Object [
        "resolveProvider", JSON_Bool comp.resolveProvider;
        "triggerCharacters", Jprint.string_array comp.completion_triggerCharacters;
      ]);
      "signatureHelpProvider", Option.map cap.signatureHelpProvider ~f:(fun shp -> JSON_Object [
        "triggerCharacters", Jprint.string_array shp.sighelp_triggerCharacters;
      ]);
      "definitionProvider", Some (JSON_Bool cap.definitionProvider);
      "referencesProvider", Some (JSON_Bool cap.referencesProvider);
      "documentHighlightProvider", Some (JSON_Bool cap.documentHighlightProvider);
      "documentSymbolProvider", Some (JSON_Bool cap.documentSymbolProvider);
      "workspaceSymbolProvider", Some (JSON_Bool cap.workspaceSymbolProvider);
      "codeActionProvider", Some (JSON_Bool cap.codeActionProvider);
      "codeLensProvider", Option.map cap.codeLensProvider ~f:(fun codelens -> JSON_Object [
        "resolveProvider", JSON_Bool codelens.codelens_resolveProvider;
      ]);
      "documentFormattingProvider", Some (JSON_Bool cap.documentFormattingProvider);
      "documentRangeFormattingProvider", Some (JSON_Bool cap.documentRangeFormattingProvider);
      "documentOnTypeFormattingProvider", Option.map
        cap.documentOnTypeFormattingProvider ~f:(fun o -> JSON_Object [
          "firstTriggerCharacter", JSON_String o.firstTriggerCharacter;
          "moreTriggerCharacter", Jprint.string_array o.moreTriggerCharacter;
        ]);
      "renameProvider", Some (JSON_Bool cap.renameProvider);
      "documentLinkProvider", Option.map cap.documentLinkProvider ~f:(fun dlp -> JSON_Object [
        "resolveProvider", JSON_Bool dlp.doclink_resolveProvider;
      ]);
      "executeCommandProvider", Option.map cap.executeCommandProvider ~f:(fun p -> JSON_Object [
        "commands", Jprint.string_array p.commands;
      ]);
      "typeCoverageProvider", Some (JSON_Bool cap.typeCoverageProvider);
      "rageProvider", Some (JSON_Bool cap.rageProvider);
    ];
  ]


(************************************************************************)
(** error response                                                     **)
(************************************************************************)

let get_error_info (e: exn) : int * string * json option =
  (* returns (lspErrorCode, friendlyMessage, lspData) *)
  match e with
  | Error.Parse message -> (-32700, message, None)
  | Error.InvalidRequest message -> (-32600, message, None)
  | Error.MethodNotFound message -> (-32601, message, None)
  | Error.InvalidParams message -> (-32602, message, None)
  | Error.InternalError message -> (-32603, message, None)
  | Error.ServerErrorStart (message, data) -> (-32099, message, Some (print_initializeError data))
  | Error.ServerErrorEnd message -> (-32000, message, None)
  | Error.ServerNotInitialized message -> (-32002, message, None)
  | Error.Unknown message -> (-32001, message, None)
  | Error.RequestCancelled message -> (-32800, message, None)
  | Exit_status.Exit_with code -> (-32001, Exit_status.to_string code, None)
  | _ -> (-32001, Printexc.to_string e, None)

let print_error (e: exn) (stack: string) : json =
  let open Hh_json in
  let (code, message, original_data) = get_error_info e in
  let stack_json_property = ("stack", string_ stack) in
  (* We'd like to add a stack-trace. The only place we can fit it, that will *)
  (* be respected by vscode-jsonrpc, is inside the 'data' field. And we can  *)
  (* do that only if data is an object. We can synthesize one if needed.     *)
  let data = match original_data with
    | None -> JSON_Object [stack_json_property]
    | Some (JSON_Object o) -> JSON_Object (stack_json_property :: o)
    | Some primitive -> primitive
  in
  JSON_Object [
    "code", int_ code;
    "message", string_ message;
    "data", data;
  ]

let parse_error (error: json) : exn =
  let json = Some error in
  let code = Jget.int_exn json "code" in
  let message = Jget.string_exn json "message" in
  let _data = Jget.val_opt json "data" in
  match code with
  | -32700 -> Error.Parse message
  | -32600 -> Error.InvalidRequest message
  | -32601 -> Error.MethodNotFound message
  | -32602 -> Error.InvalidParams message
  | -32603 -> Error.InternalError message
  | -32800 -> Error.RequestCancelled message
  | -32001 -> Error.Unknown message
  | -32099 (* Error.ServerErrorStart *)
  | -32000 (* Error.ServerErrorEnd *)
  | -32002 (* Error.ServerNotInitialized *)
  | _ -> raise (Error.Parse ("Unrecognized LSP error code: " ^ (string_of_int code)))


(************************************************************************)
(** universal parser+printer                                           **)
(************************************************************************)

let request_name_to_string (request: lsp_request) : string =
  match request with
  | ShowMessageRequestRequest _ -> "window/showMessageRequest"
  | ShowStatusRequest _ -> "window/showStatus"
  | InitializeRequest _ -> "initialize"
  | ShutdownRequest -> "shutdown"
  | HoverRequest _ -> "textDocument/hover"
  | CompletionRequest _ -> "textDocument/completion"
  | CompletionItemResolveRequest _ -> "completionItem/resolve"
  | DefinitionRequest _ -> "textDocument/definition"
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
  | UnknownRequest (method_, _params) -> method_

let result_name_to_string (result: lsp_result) : string =
  match result with
  | ShowMessageRequestResult _ -> "window/showMessageRequest"
  | ShowStatusResult _ -> "window/showStatus"
  | InitializeResult _ -> "initialize"
  | ShutdownResult -> "shutdown"
  | HoverResult _ -> "textDocument/hover"
  | CompletionResult _ -> "textDocument/completion"
  | CompletionItemResolveResult _ -> "completionItem/resolve"
  | DefinitionResult _ -> "textDocument/definition"
  | WorkspaceSymbolResult _ -> "workspace/symbol"
  | DocumentSymbolResult _ -> "textDocument/documentSymbol"
  | FindReferencesResult _ -> "textDocument/references"
  | DocumentHighlightResult _ -> "textDocument/documentHighlight"
  | TypeCoverageResult _ -> "textDocument/typeCoverage"
  | DocumentFormattingResult _ -> "textDocument/formatting"
  | DocumentRangeFormattingResult _ -> "textDocument/rangeFormatting"
  | DocumentOnTypeFormattingResult _ -> "textDocument/onTypeFormatting"
  | RageResult _ -> "telemetry/rage"
  | RenameResult _ -> "textDocument/rename"
  | ErrorResult (e, _stack) -> "ERROR/" ^ (Printexc.to_string e)

let notification_name_to_string (notification: lsp_notification) : string =
  match notification with
  | ExitNotification -> "exit"
  | CancelRequestNotification _ -> "$/cancelRequest"
  | PublishDiagnosticsNotification _ -> "textDocument/publishDiagnostics"
  | DidOpenNotification _ -> "textDocument/didOpen"
  | DidCloseNotification _ -> "textDocument/didClose"
  | DidSaveNotification _ -> "textDocument/didSave"
  | DidChangeNotification _ -> "textDocument/didChange"
  | TelemetryNotification _ -> "telemetry/event"
  | LogMessageNotification _ -> "window/logMessage"
  | ShowMessageNotification _ -> "window/showMessage"
  | ProgressNotification _ -> "window/progress"
  | ActionRequiredNotification _ -> "window/actionRequired"
  | ConnectionStatusNotification _ -> "telemetry/connectionStatus"
  | UnknownNotification (method_, _params) -> method_

let message_name_to_string (message: lsp_message) : string =
  match message with
  | RequestMessage (_, r) -> request_name_to_string r
  | NotificationMessage n -> notification_name_to_string n
  | ResponseMessage (_, r) -> result_name_to_string r

let denorm_message_to_string (message: lsp_message) : string =
  match message with
  | RequestMessage (id, r) ->
    Printf.sprintf "request %s %s" (id_to_string id) (request_name_to_string r)
  | NotificationMessage n ->
    Printf.sprintf "notification %s" (notification_name_to_string n)
  | ResponseMessage (id, ErrorResult (e, _stack)) ->
    Printf.sprintf "error %s %s" (id_to_string id) (Printexc.to_string e)
  | ResponseMessage (id, r) ->
    Printf.sprintf "result %s %s" (id_to_string id) (result_name_to_string r)

let parse_lsp_request (method_: string) (params: json option) : lsp_request =
  match method_ with
  | "initialize" -> InitializeRequest (parse_initialize params)
  | "shutdown" -> ShutdownRequest
  | "textDocument/hover" -> HoverRequest (parse_hover params)
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
  | "telemetry/rage" -> RageRequest
  | "completionItem/resolve"
  | "window/showMessageRequest"
  | "window/showStatus"
  | _ -> UnknownRequest (method_, params)

let parse_lsp_notification (method_: string) (params: json option) : lsp_notification =
  match method_ with
  | "$/cancelRequest" -> CancelRequestNotification (parse_cancelRequest params)
  | "exit" -> ExitNotification
  | "textDocument/didOpen" -> DidOpenNotification (parse_didOpen params)
  | "textDocument/didClose" -> DidCloseNotification (parse_didClose params)
  | "textDocument/didSave" -> DidSaveNotification (parse_didSave params)
  | "textDocument/didChange" -> DidChangeNotification (parse_didChange params)
  | "textDocument/publishDiagnostics"
  | "window/logMessage"
  | "window/showMessage"
  | "window/progress"
  | "window/actionRequired"
  | "telemetry/connectionStatus"
  | _ -> UnknownNotification (method_, params)

let parse_lsp_result (request: lsp_request) (result: json) : lsp_result =
  let method_ = request_name_to_string request in
  match request with
  | ShowMessageRequestRequest _ ->
    ShowMessageRequestResult (parse_result_showMessageRequest (Some result))
  | ShowStatusRequest _ ->
    ShowStatusResult (parse_result_showMessageRequest (Some result)) (* shares result type *)
  | InitializeRequest _
  | ShutdownRequest
  | HoverRequest _
  | CompletionRequest _
  | CompletionItemResolveRequest _
  | DefinitionRequest _
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
  | UnknownRequest _ ->
    raise (Error.Parse ("Don't know how to parse LSP response " ^ method_))

(* parse_lsp: non-jsonrpc inputs - will raise an exception                    *)
(* requests and notifications - will raise an exception if they're malformed, *)
(*   otherwise return Some                                                    *)
(* responses - will raise an exception if they're malformed, will return None *)
(*   if they're absent from the "outstanding" map, otherwise return Some.     *)
let parse_lsp (json: json) (outstanding: lsp_id -> lsp_request) : lsp_message =
  let json = Some json in
  let id = Jget.val_opt json "id" |> parse_id_opt in
  let method_opt = Jget.string_opt json "method" in
  let params = Jget.val_opt json "params" in
  let result = Jget.val_opt json "result" in
  let error = Jget.val_opt json "error" in
  match id, method_opt, result, error with
  | None, Some method_, _, _ -> NotificationMessage (parse_lsp_notification method_ params)
  | Some id, Some method_, _, _ -> RequestMessage (id, parse_lsp_request method_ params)
  | Some id, _, Some result, _ ->
    let request = outstanding id in ResponseMessage (id, parse_lsp_result request result)
  | Some id, _, _, Some error  -> ResponseMessage (id, ErrorResult (parse_error error, ""))
  | _, _, _, _ -> raise (Error.Parse "Not JsonRPC")


let print_lsp_request (id: lsp_id) (request: lsp_request) : json =
  let method_ = request_name_to_string request in
  let params = match request with
    | ShowMessageRequestRequest r -> print_showMessageRequest r
    | ShowStatusRequest r -> print_showStatus r
    | InitializeRequest _
    | ShutdownRequest
    | HoverRequest _
    | CompletionRequest _
    | CompletionItemResolveRequest _
    | DefinitionRequest _
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
    | UnknownRequest _ ->
      failwith ("Don't know how to print request " ^ method_)
  in
  JSON_Object [
    "jsonrpc", JSON_String "2.0";
    "id", print_id id;
    "method", JSON_String method_;
    "params", params;
  ]

let print_lsp_response (id: lsp_id) (result: lsp_result) : json =
  let method_ = result_name_to_string result in
  let json = match result with
    | InitializeResult r -> print_initialize r
    | ShutdownResult -> print_shutdown ()
    | HoverResult r -> print_hover r
    | CompletionResult r -> print_completion r
    | DefinitionResult r -> print_definition r
    | WorkspaceSymbolResult r -> print_workspaceSymbol r
    | DocumentSymbolResult r -> print_documentSymbol r
    | FindReferencesResult r -> print_findReferences r
    | DocumentHighlightResult r -> print_documentHighlight r
    | TypeCoverageResult r -> print_typeCoverage r
    | DocumentFormattingResult r -> print_documentFormatting r
    | DocumentRangeFormattingResult r -> print_documentRangeFormatting r
    | DocumentOnTypeFormattingResult r -> print_documentOnTypeFormatting r
    | RageResult r -> print_rage r
    | RenameResult r -> print_documentRename r
    | ShowMessageRequestResult _
    | ShowStatusResult _
    | CompletionItemResolveResult _ ->
      failwith ("Don't know how to print result " ^ method_)
    | ErrorResult (e, stack) -> print_error e stack
  in
  match result with
  | ErrorResult _ ->
    JSON_Object [
      "jsonrpc", JSON_String "2.0";
      "id", print_id id;
      "error", json;
    ]
  | _ ->
    JSON_Object [
      "jsonrpc", JSON_String "2.0";
      "id", print_id id;
      "result", json;
    ]

let print_lsp_notification (notification: lsp_notification) : json =
  let method_ = notification_name_to_string notification in
  let params = match notification with
    | CancelRequestNotification r -> print_cancelRequest r
    | PublishDiagnosticsNotification r -> print_diagnostics r
    | TelemetryNotification r -> print_logMessage r.LogMessage.type_ r.LogMessage.message
    | LogMessageNotification r -> print_logMessage r.LogMessage.type_ r.LogMessage.message
    | ShowMessageNotification r -> print_showMessage r.ShowMessage.type_ r.ShowMessage.message
    | ProgressNotification r -> print_progress r.Progress.id r.Progress.label
    | ActionRequiredNotification r ->
        print_actionRequired r.ActionRequired.id r.ActionRequired.label
    | ConnectionStatusNotification r -> print_connectionStatus r
    | ExitNotification
    | DidOpenNotification _
    | DidCloseNotification _
    | DidSaveNotification _
    | DidChangeNotification _
    | UnknownNotification _ ->
      failwith ("Don't know how to print notification " ^ method_)
  in
  JSON_Object [
    "jsonrpc", JSON_String "2.0";
    "method", JSON_String method_;
    "params", params;
  ]

let print_lsp (message: lsp_message) : json =
  match message with
  | RequestMessage (id, request) -> print_lsp_request id request
  | ResponseMessage (id, result) -> print_lsp_response id result
  | NotificationMessage notification -> print_lsp_notification notification
