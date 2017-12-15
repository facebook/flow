(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open Hh_core
open Lsp
open Hh_json


(************************************************************************)
(** Helpers for parsing & printing                                     **)
(************************************************************************)

module Jget = struct
  (* Helpers for the various "option" monads in use for Json, to succinctly
     capture the spirit of JSON (tolerance for missing values) and the spirit
     of LSP (loads of nested optional members with obvious defaults)
     and the usefuless of error-checking (in case a required field is absent)...
     - We use "json option" throughout. Things which you might expect to return
       a json are instead lifted to return a json option, so you can use all the
       accessors on them more easily. When you attempt to get "o.m", either
       it's present both because "o" is Some, and because "m" is a member.
       Or it's absent because either of those things is false...
     - The "_opt" accessors uniformally return Some (present) or None (absent),
       regardless of which of the two things caused absence.
     - The "_d" accessors uniformally return a value (present) or default.
     - The "_exn" accessors uniformally return a value (present) or throw.

     The effect of this is you lose precise information about what exactly
     caused an absence (which is usually only of marginal benefit), and in
     return you gain a consistent way to handle both optionals and requireds.
     Note: if you wish to get an int, and it's present but not parseable as
     an int, then this always throws.
  *)

  let get_opt hhjson_getter json key = match json with
    | None -> None
    | Some json -> match hhjson_getter key (json, []) with
      | Ok (r, _keytrace) -> Some r
      | _ -> None

  let get_exn opt_getter json key = match opt_getter json key with
    | None -> raise (Error.Parse key)
    | Some v -> v

  let int_string_opt (s: string option) : int option = match s with
    | None -> None
    | Some s ->
      try Some (int_of_string s)
      with Failure _ -> raise (Error.Parse ("not an int: " ^ s))

  let list_opt (l: 'a list option) : 'a option list option = match l with
    | None -> None
    | Some x -> Some (List.map x ~f:(fun a -> Some a))

  (* Accessors which return None on absence *)
  let string_opt = get_opt Access.get_string
  let bool_opt = get_opt Access.get_bool
  let obj_opt = get_opt Access.get_obj
  let val_opt = get_opt Access.get_val
  let int_opt json key = get_opt Access.get_number json key |> int_string_opt
  let array_opt json key = get_opt Access.get_array json key |> list_opt
  (* array_opt lifts all the array's members into the "json option" monad *)

  (* Accessors which return a supplied default on absence *)
  let string_d json key ~default = Option.value (string_opt json key) ~default
  let bool_d json key ~default = Option.value (bool_opt json key) ~default
  let int_d json key ~default = Option.value (int_opt json key) ~default
  let array_d json key ~default = Option.value (array_opt json key) ~default

  (* Accessors which throw "Error.Parse key" on absence *)
  let string_exn = get_exn string_opt
  let int_exn = get_exn int_opt
  let obj_exn json key = Some (get_exn obj_opt json key)
  (* obj_exn lifts the result into the "json option" monad *)
end

module Jprint = struct
  (* object_opt is like Hh_json.JSON_Object constructor except it takes
     key * (value option): if a value is None, then it omits this member. *)
  let object_opt (keyvalues : (string * (json option)) list) : json =
    let make_pair_option = function
      | (k, Some v) -> Some (k,v)
      | _ -> None
    in
    JSON_Object (List.filter_map keyvalues ~f:make_pair_option)

  (* Convenience function to convert string list to JSON_Array *)
  let string_array (l: string list) : json =
    JSON_Array (List.map l ~f:string_)
end


(************************************************************************)
(** Miscellaneous LSP structures                                       **)
(************************************************************************)

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
    insertSpaces = Jget.bool_d json "insertSpaces" false;
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

let print_shutdown (_r: Shutdown.result) : json =
  JSON_Null


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
(** textDocument/publishDiagnostics notification                       **)
(************************************************************************)

let print_diagnostics (r: PublishDiagnostics.params) : json =
  let open PublishDiagnostics in
  let print_diagnosticSeverity = function
    | PublishDiagnostics.Error -> int_ 1
    | PublishDiagnostics.Warning -> int_ 2
    | PublishDiagnostics.Information -> int_ 3
    | PublishDiagnostics.Hint -> int_ 4
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
      "code", Option.map diagnostic.code int_;
      "source", Option.map diagnostic.source string_;
      "message", Some (JSON_String diagnostic.message);
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

let print_showMessageRequest
    (type_: MessageType.t)
    (message: string)
    (titles: string list)
  : json =
  let open ShowMessageRequest in
  let print_action (action: messageActionItem) : json =
    JSON_Object [
      "title", JSON_String action.title;
    ]
  in
  let actions = List.map titles ~f:(fun title -> { title; }) in
  let r = { type_; message; actions; } in
  JSON_Object [
    "type", print_messageType r.type_;
    "message", JSON_String r.message;
    "actions", JSON_Array (List.map r.actions ~f:print_action);
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
(** textDocument/hover request                                         **)
(************************************************************************)

let parse_hover (params: json option) : Hover.params =
  parse_textDocumentPositionParams params

let print_hover (r: Hover.result) : json =
  let open Hover in
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
  JSON_Array (List.map r ~f:print_location)


(************************************************************************)
(** textDocument/completion request                                    **)
(************************************************************************)

let parse_completion (params: json option) : Definition.params =
  parse_textDocumentPositionParams params

let print_completion (r: Completion.result) : json =
  let open Completion in
  let rec print_completionItem (item: Completion.completionItem) : json =
    Jprint.object_opt [
      "label", Some (JSON_String item.label);
      "kind", Option.map item.kind print_completionItemKind;
      "detail", Option.map item.detail string_;
      "inlineDetail", Option.map item.inlineDetail string_;
      "itemType", Option.map item.itemType string_;
      "documentation", Option.map item.documentation string_;
      "sortText", Option.map item.sortText string_;
      "filterText", Option.map item.filterText string_;
      "insertText", Option.map item.insertText string_;
      "insertTextFormat", Some (print_insertFormat item.insertTextFormat);
      "textEdit", Option.map (List.hd item.textEdits) print_textEdit;
      "additionalTextEdit", (match (List.tl item.textEdits) with
        | None | Some [] -> None
        | Some l -> Some (JSON_Array (List.map l ~f:print_textEdit)));
      "command", Option.map item.command print_command;
      "data", item.data;
    ]
  and print_completionItemKind = function
    | Text -> int_ 1
    | Method -> int_ 2
    | Function -> int_ 3
    | Constructor -> int_ 4
    | Field -> int_ 5
    | Variable -> int_ 6
    | Class -> int_ 7
    | Interface -> int_ 8
    | Module -> int_ 9
    | Property -> int_ 10
    | Unit -> int_ 11
    | Value -> int_ 12
    | Enum -> int_ 13
    | Keyword -> int_ 14
    | Snippet -> int_ 15
    | Color -> int_ 16
    | File -> int_ 17
    | Reference -> int_ 18
  and print_insertFormat = function
    | PlainText -> int_ 1
    | SnippetFormat -> int_ 2
  in
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
  let open TextDocumentPositionParams in
  let as_positionParams = parse_textDocumentPositionParams params in
  let context = Jget.obj_opt params "context" in
  { FindReferences.
    textDocument = as_positionParams.textDocument;
    position = as_positionParams.position;
    context =
      { FindReferences.
        includeDeclaration = Jget.bool_d context "includeDeclaration" true;
      }
  }

let print_findReferences (r: Location.t list) : json =
  JSON_Array (List.map r ~f:print_location)


(************************************************************************)
(** textDocument/documentHighlights request                            **)
(************************************************************************)

let parse_documentHighlights (params: json option)
  : DocumentHighlights.params =
  parse_textDocumentPositionParams params

let print_documentHighlights (r: DocumentHighlights.result) : json =
  let open DocumentHighlights in
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
    JSON_Object [
      "range", print_range uncov.range;
      "message", string_ uncov.message;
    ]
  in
  JSON_Object [
    "coveredPercent", int_ r.coveredPercent;
    "uncoveredRanges", JSON_Array (List.map r.uncoveredRanges ~f:print_uncov)
  ]


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
      client_capabilities = Jget.obj_opt json "capabilities"
                            |> parse_capabilities;
      trace = Jget.string_opt json "trace" |> parse_trace;
    }
  and parse_trace (s : string option) : trace = match s with
    | Some "messages" -> Messages
    | Some "verbose" -> Verbose
    | _ -> Off
  and parse_capabilities json =
    {
      workspace = Jget.obj_opt json "workspace" |> parse_workspace;
      textDocument = Jget.obj_opt json "textDocument" |> parse_textDocument;
      window = Jget.obj_opt json "window" |> parse_window;
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
      progress = Jget.obj_opt json "progress" |> Option.is_some;
      actionRequired = Jget.obj_opt json "actionRequired" |> Option.is_some;
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
  | Error.ServerErrorStart (message, data) -> (-32603, message, Some (print_initializeError data))
  | Error.ServerErrorEnd message -> (-32000, message, None)
  | Error.ServerNotInitialized message -> (-32002, message, None)
  | Error.Unknown message -> (-32001, message, None)
  | Error.RequestCancelled message -> (-32800, message, None)
  | Exit_status.Exit_with code -> (-32001, Exit_status.to_string code, None)
  | _ -> (-32001, Printexc.to_string e, None)

let print_error (e: exn) (stack: string): json =
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
