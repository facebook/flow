(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

(**
 * This file is an OCaml representation of the Language Server Protocol
 * https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md
 * based on the current v3.
 *
 * Changes to make it more natural in OCaml:
 * - We don't represent the common base types of Requests/Errors/Notifications
 *   because base types don't naturally mix with abstract data types, and
 *   because code for these things is done more naturally at the JSON layer
 * - We avoid option types where we can. The idea is to follow the internet
 *   "robustness" rule of being liberal in what we accept, conservative in
 *   what we emit: if we're parsing a message and it lacks a field, and if
 *   the spec tells us how to interpret absence, then we do that interpretation
 *   at the JSON->LSP parsing level (so long as the interpretation is lossless).
 *   On the emitting side, we might as well emit all fields.
 * - For every request, like Initialize or workspace/Symbol, we've invented
 *   "Initialize.response = (Initialize.result, Initialize.error) Result"
 *   or "Symbol.response = (Symbol.result, Error.error) Result" to show
 *   the two possible return types from this request. Note that each different
 *   request can have its own custom error type, although most don't.
 * - Most datatypes go in modules since there are so many name-clashes in
 *   the protocol and OCaml doesn't like name-clashes. Only exceptions are
 *   the really primitive types like location and documentUri.
 *   The few places where we still had to rename fields to avoid OCaml name
 *   clashes I've noted in the comments with the word "wire" to indicate the
 *   over-the-wire form of the name.
 * - Names have been translated from jsonConvention into ocaml convention
 *   only where necessary, e.g. because ocaml uses lowercase for types.
 * - The spec has space for extra fields like "experimental". It obviously
 *   doesn't make sense to encode them in a type system. I've omitted them
 *   entirely.
*)


type documentUri = string

(* A position is between two characters like an 'insert' cursor in a editor *)
type position = {
  line: int;  (* line position in a document [zero-based] *)
  character: int;  (* character offset on a line in a document [zero-based] *)
}

(* A range is comparable to a selection in an editor *)
type range = {
  start: position;  (* the range's start position *)
  end_: position;  (* the range's end position [exclusive] *)
}

(* Represents a location inside a resource, such as a line inside a text file *)
module Location = struct
  type t = {
    uri: documentUri;
    range: range;
  }
end

(* markedString can be used to render human readable text. It is either a
 * markdown string or a code-block that provides a language and a code snippet.
 * Note that markdown strings will be sanitized by the client - including
 * escaping html *)
type markedString =
  | MarkedString of string
  | MarkedCode of string * string (* lang, value *)

(* Represents a reference to a command. Provides a title which will be used to
 * represent a command in the UI. Commands are identitifed using a string
 * identifier and the protocol currently doesn't specify a set of well known
 * commands. So executing a command requires some tool extension code. *)
module Command = struct
  type t = {
    title: string;  (* title of the command, like `save` *)
    command: string;  (* the identifier of the actual command handler *)
    arguments: Hh_json.json list;  (* wire: it can be omitted *)
  }
end

(* A textual edit applicable to a text document. If n textEdits are applied
   to a text document all text edits describe changes to the initial document
   version. Execution wise text edits should applied from the bottom to the
   top of the text document. Overlapping text edits are not supported. *)
module TextEdit = struct
  type t = {
    range: range;  (* to insert text, use a range where start = end *)
    newText: string;  (* for delete operations, use an empty string *)
  }
end

(* Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
  }
end

(* An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    version: int;  (* the version number of this document *)
  }
end

(* Describes textual changes on a single text document. The text document is
   referred to as a VersionedTextDocumentIdentifier to allow clients to check
   the text document version before an edit is applied. *)
module TextDocumentEdit = struct
  type t = {
    textDocument: VersionedTextDocumentIdentifier.t;
    edits: TextEdit.t list;
  }
end

(* A workspace edit represents changes to many resources managed in the
   workspace. A workspace edit now consists of an array of TextDocumentEdits
   each describing a change to a single text document. *)
module WorkspaceEdit = struct
  type t = {
    changes: TextDocumentEdit.t list;  (* holds changes to existing docs *)
  }
end

(* An item to transfer a text document from the client to the server. The
   version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t = {
    uri: documentUri;  (* the text document's URI *)
    languageId: string;  (* the text document's language identifier *)
    version: int;  (* the version of the document *)
    text: string;  (* the content of the opened text document *)
  }
end

(* A parameter literal used in requests to pass a text document and a position
   inside that document. *)
module TextDocumentPositionParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
  }
end

(* A document filter denotes a document through properties like language,
   schema or pattern. E.g. language:"typescript",scheme:"file"
   or langauge:"json",pattern:"**/package.json" *)
module DocumentFilter = struct
  type t = {
    language: string option;  (* a language id, like "typescript" *)
    scheme: string option;  (* a uri scheme, like "file" or "untitled" *)
    pattern: string option;  (* a glob pattern, like "*.{ts,js}" *)
  }
end

(* A document selector is the combination of one or many document filters. *)
module DocumentSelector = struct
  type t = DocumentFilter.t list
end


(* Represents information about programming constructs like variables etc. *)
module SymbolInformation = struct
  type t = {
    name: string;
    kind: symbolKind;
    location: Location.t;  (* the location of the symbol token itself *)
    containerName: string option;  (* the symbol containing this symbol *)
  }

  and symbolKind =
    | File  (* 1 *)
    | Module  (* 2 *)
    | Namespace  (* 3 *)
    | Package  (* 4 *)
    | Class  (* 5 *)
    | Method  (* 6 *)
    | Property  (* 7 *)
    | Field  (* 8 *)
    | Constructor  (* 9 *)
    | Enum  (* 10 *)
    | Interface  (* 11 *)
    | Function  (* 12 *)
    | Variable  (* 13 *)
    | Constant  (* 14 *)
    | String  (* 15 *)
    | Number  (* 16 *)
    | Boolean  (* 17 *)
    | Array  (* 18 *)
end


(* For showing messages (not diagnostics) in the user interface. *)
module MessageType = struct
  type t =
    | ErrorMessage  (* 1 *)
    | WarningMessage  (* 2 *)
    | InfoMessage  (* 3 *)
    | LogMessage  (* 4 *)
end


(* Cancellation notification, method="$/cancelRequest" *)
module CancelRequest = struct
  type params = cancelParams

  and cancelParams = {
    id: string;  (* the request id to cancel *)
  }
end

(* Initialize request, method="initialize" *)
module Initialize = struct
  type params = {
    processId: int option;  (* pid of parent process *)
    rootPath: string option;  (* deprecated *)
    rootUri: documentUri option;  (* the root URI of the workspace *)
    (* omitted: initiaization_options *)
    client_capabilities: client_capabilities;  (* "capabilities" over wire *)
    trace: trace;  (* the initial trace setting, default="off" *)
  }

  and result = {
    server_capabilities: server_capabilities; (* "capabilities" over wire *)
  }

  and errorData = {
    retry: bool;  (* should client retry the initialize request *)
  }

  and trace =
    | Off
    | Messages
    | Verbose

  and client_capabilities = {
    workspace: workspaceClientCapabilities;
    textDocument: textDocumentClientCapabilities;
    window: windowClientCapabilities;
    (* omitted: experimental *)
  }

  and workspaceClientCapabilities = {
    applyEdit: bool;  (* client supports appling batch edits *)
    workspaceEdit: workspaceEdit;
    (* omitted: dynamic-registration fields *)
  }

  and workspaceEdit = {
    documentChanges: bool;  (* client supports versioned doc changes *)
  }

  and textDocumentClientCapabilities = {
    synchronization: synchronization;
    completion: completion;  (* textDocument/completion *)
    (* omitted: dynamic-registration fields *)
  }

  (* synchronization capabilities say what messages the client is capable
   * of sending, should be be so asked by the server.
   * We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)
  and synchronization = {
    can_willSave: bool;  (* client can send textDocument/willSave *)
    can_willSaveWaitUntil: bool;  (* textDoc.../willSaveWaitUntil *)
    can_didSave: bool;  (* textDocument/didSave *)
  }

  and completion = {
    completionItem: completionItem;
  }

  and completionItem = {
    snippetSupport: bool;  (* client can do snippets as insert text *)
  }

  and windowClientCapabilities = {
    progress: bool;  (* Nuclide-specific: client supports window/progress *)
    actionRequired: bool;  (* Nuclide-specific: client supports window/actionRequired *)
  }

  (* What capabilities the server provides *)
  and server_capabilities = {
    textDocumentSync: textDocumentSyncOptions; (* how to sync *)
    hoverProvider: bool;
    completionProvider: completionOptions option;
    signatureHelpProvider: signatureHelpOptions option;
    definitionProvider: bool;
    referencesProvider: bool;
    documentHighlightProvider: bool;
    documentSymbolProvider: bool;  (* ie. document outline *)
    workspaceSymbolProvider: bool;  (* ie. find-symbol-in-project *)
    codeActionProvider: bool;
    codeLensProvider: codeLensOptions option;
    documentFormattingProvider: bool;
    documentRangeFormattingProvider: bool;
    documentOnTypeFormattingProvider: documentOnTypeFormattingOptions option;
    renameProvider: bool;
    documentLinkProvider: documentLinkOptions option;
    executeCommandProvider: executeCommandOptions option;
    typeCoverageProvider: bool;  (* Nuclide-specific feature *)
    rageProvider: bool;
    (* omitted: experimental *)
  }

  and completionOptions = {
    resolveProvider: bool;  (* server resolves extra info on demand *)
    completion_triggerCharacters: string list; (* wire "triggerCharacters" *)
  }

  and signatureHelpOptions = {
    sighelp_triggerCharacters: string list; (* wire "triggerCharacters" *)
  }

  and codeLensOptions = {
    codelens_resolveProvider: bool;  (* wire "resolveProvider" *)
  }

  and documentOnTypeFormattingOptions = {
    firstTriggerCharacter: string;  (* e.g. "}" *)
    moreTriggerCharacter: string list;
  }

  and documentLinkOptions = {
    doclink_resolveProvider: bool;  (* wire "resolveProvider" *)
  }

  and executeCommandOptions = {
    commands: string list;  (* the commands to be executed on the server *)
  }

  (* text document sync options say what messages the server requests the
   * client to send. We use the "want_" prefix for OCaml naming reasons;
   * this prefix is absent in LSP. *)
  and textDocumentSyncOptions = {
    want_openClose: bool;  (* textDocument/didOpen+didClose *)
    want_change: textDocumentSyncKind;
    want_willSave: bool;  (* textDocument/willSave *)
    want_willSaveWaitUntil: bool;  (* textDoc.../willSaveWaitUntil *)
    want_didSave: saveOptions option;  (* textDocument/didSave *)
  }

  and textDocumentSyncKind =
    | NoSync (* 0 *)  (* docs should not be synced at all. Wire "None" *)
    | FullSync (* 1 *)  (* synced by always sending full content. Wire "Full" *)
    | IncrementalSync (* 2 *)  (* full only on open. Wire "Incremental" *)

  and saveOptions = {
    includeText: bool;  (* the client should include content on save *)
  }
end

(* Shutdown request, method="shutdown" *)
module Shutdown = struct
  type result = unit
end

(* Exit notification, method="exit" *)
module Exit = struct
end

(* Rage request, method="telemetry/rage" *)
module Rage = struct
  type result = rageItem list

  and rageItem = {
    title: string option;
    data: string;
  }
end


(* Hover request, method="textDocument/hover" *)
module Hover = struct
  type params = TextDocumentPositionParams.t

  and result = {
    contents: markedString list; (* wire: either a single one or an array *)
    range: range option;
  }
end

(* PublishDiagnostics notification, method="textDocument/PublishDiagnostics" *)
module PublishDiagnostics = struct
  type params = publishDiagnosticsParams

  and publishDiagnosticsParams = {
    uri: documentUri;
    diagnostics: diagnostic list;
  }

  and diagnostic = {
    range: range;  (* the range at which the message applies *)
    severity: diagnosticSeverity option;  (* if omitted, client decides *)
    code: int option;  (* the diagnostic's code. Wire: can be string too *)
    source: string option;  (* human-readable string, eg. typescript/lint *)
    message: string;  (* the diagnostic's message *)
    relatedLocations: relatedLocation list;
  }

  and diagnosticSeverity =
    | Error (* 1 *)
    | Warning (* 2 *)
    | Information (* 3 *)
    | Hint (* 4 *)

  and relatedLocation = {
    relatedLocation: Location.t;  (* wire: just "location" *)
    relatedMessage: string;  (* wire: just "message" *)
  }
end

(* DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = {
    textDocument: TextDocumentItem.t;  (* the document that was opened *)
  }
end

(* DidCloseTextDocument notification, method="textDocument/didClose" *)
module DidClose = struct
  type params = didCloseTextDocumentParams

  and didCloseTextDocumentParams = {
    textDocument: TextDocumentIdentifier.t; (* the doc that was closed *)
  }
end

(* DidSaveTextDocument notification, method="textDocument/didSave" *)
module DidSave = struct
  type params = didSaveTextDocumentParams

  and didSaveTextDocumentParams = {
    textDocument: TextDocumentIdentifier.t; (* the doc that was saved *)
    text: string option; (* content when saved; depends on includeText *)
  }
end

(* DidChangeTextDocument notification, method="textDocument/didChange" *)
module DidChange = struct
  type params = didChangeTextDocumentParams

  and didChangeTextDocumentParams = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanges: textDocumentContentChangeEvent list;
  }

  and textDocumentContentChangeEvent = {
    range: range option; (* the range of the document that changed *)
    rangeLength: int option; (* the length that got replaced *)
    text: string; (* the new text of the range/document *)
  }
end

(* Goto Definition request, method="textDocument/definition" *)
module Definition = struct
  type params = TextDocumentPositionParams.t

  and result = Location.t list  (* wire: either a single one or an array *)
end

(* Completion request, method="textDocument/completion" *)
module Completion = struct
  type params = TextDocumentPositionParams.t

  and result = completionList  (* wire: can also be 'completionItem list' *)

  and completionList = {
    isIncomplete: bool; (* further typing should result in recomputing *)
    items: completionItem list;
  }

  and completionItem = {
    label: string;  (* the label in the UI *)
    kind: completionItemKind option;  (* tells editor which icon to use *)
    detail: string option;  (* human-readable string like type/symbol info *)
    inlineDetail: string option; (* nuclide-specific, right column *)
    itemType: string option; (* nuclide-specific, left column *)
    documentation: string option;  (* human-readable doc-comment *)
    sortText: string option;  (* used for sorting; if absent, uses label *)
    filterText: string option;  (* used for filtering; if absent, uses label *)
    insertText: string option;  (* used for inserting; if absent, uses label *)
    insertTextFormat: insertTextFormat;
    textEdits: TextEdit.t list;  (* wire: split into hd and tl *)
    command: Command.t option;  (* if present, is executed after completion *)
    data: Hh_json.json option;
  }

  and completionItemKind =
    | Text (* 1 *)
    | Method (* 2 *)
    | Function (* 3 *)
    | Constructor (* 4 *)
    | Field (* 5 *)
    | Variable (* 6 *)
    | Class (* 7 *)
    | Interface (* 8 *)
    | Module (* 9 *)
    | Property (* 10 *)
    | Unit (* 11 *)
    | Value (* 12 *)
    | Enum (* 13 *)
    | Keyword (* 14 *)
    | Snippet (* 15 *)
    | Color (* 16 *)
    | File (* 17 *)
    | Reference (* 18 *)

  and insertTextFormat =
    | PlainText (* 1 *)  (* the insertText/textEdits are just plain strings *)
    | SnippetFormat (* 2 *)  (* wire: just "Snippet" *)
end


(* Completion Item Resolve request, method="completionItem/resolve" *)
module CompletionItemResolve = struct
  type params = Completion.completionItem

  and result = Completion.completionItem
end


(* Workspace Symbols request, method="workspace/symbol" *)
module WorkspaceSymbol = struct
  type params = workspaceSymbolParams

  and result = SymbolInformation.t list

  and workspaceSymbolParams = {
    query: string;  (* a non-empty query string *)
  }
end


(* Document Symbols request, method="textDocument/documentSymbols" *)
module DocumentSymbol = struct
  type params = documentSymbolParams

  and result = SymbolInformation.t list

  and documentSymbolParams = {
    textDocument: TextDocumentIdentifier.t;
  }
end


(* Find References request, method="textDocument/references" *)
module FindReferences = struct
  type params = referenceParams

  and result = Location.t list

  (* Following structure is an extension of TextDocumentPositionParams.t *)
  (* but we don't have nice inherantice in OCaml so we duplicate fields *)
  and referenceParams = {
    textDocument: TextDocumentIdentifier.t;  (* the text document *)
    position: position;  (* the position inside the text document *)
    context: referenceContext;
  }

  and referenceContext = {
    includeDeclaration: bool;  (* include declaration of current symbol *)
  }
end


(* Document Highlights request, method="textDocument/documentHighlight" *)
module DocumentHighlights = struct
  type params = TextDocumentPositionParams.t

  and result = documentHighlight list

  and documentHighlight = {
    range: range;  (* the range this highlight applies to *)
    kind: documentHighlightKind option;
  }

  and documentHighlightKind =
    | Text (* 1 *)  (* a textual occurrence *)
    | Read (* 2 *)  (* read-access of a symbol, like reading a variable *)
    | Write (* 3 *)  (* write-access of a symbol, like writing a variable *)
end


(* Type Coverage request, method="textDocument/typeCoverage" *)
(* THIS IS A NUCLIDE-SPECIFIC EXTENSION TO LSP.              *)
module TypeCoverage = struct
  type params = typeCoverageParams

  and result = {
    coveredPercent: int;
    uncoveredRanges: uncoveredRange list;
  }

  and typeCoverageParams = {
    textDocument: TextDocumentIdentifier.t;
  }

  and uncoveredRange = {
    range: range;
    message: string;
  }
end


(* Document Formatting request, method="textDocument/formatting" *)
module DocumentFormatting = struct
  type params = documentFormattingParams

  and result = TextEdit.t list

  and documentFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    options: formattingOptions;
  }

  and formattingOptions = {
    tabSize: int;  (* size of a tab in spaces *)
    insertSpaces: bool;  (* prefer spaces over tabs *)
    (* omitted: signature for further properties *)
  }
end


(* Document Range Formatting request, method="textDocument/rangeFormatting" *)
module DocumentRangeFormatting = struct
  type params = documentRangeFormattingParams

  and result = TextEdit.t list

  and documentRangeFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    range: range;
    options: DocumentFormatting.formattingOptions;
  }
end


(* Document On Type Formatting req., method="textDocument/onTypeFormatting" *)
module DocumentOnTypeFormatting = struct
  type params = documentOnTypeFormattingParams

  and result = TextEdit.t list

  and documentOnTypeFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    position: position;  (* the position at which this request was sent *)
    ch: string;  (* the character that has been typed *)
    options: DocumentFormatting.formattingOptions;
  }
end


(* LogMessage notification, method="window/logMessage" *)
module LogMessage = struct
  type params = logMessageParams

  and logMessageParams = {
    type_: MessageType.t;
    message: string;
  }
end


(* ShowMessage notification, method="window/showMessage" *)
module ShowMessage = struct
  type params = showMessageParams

  and showMessageParams = {
    type_: MessageType.t;
    message: string;
  }
end


(* ShowMessage request, method="window/showMessageRequest" *)
module ShowMessageRequest = struct
  type t = Some of {cancel: unit -> unit;} | None

  and params = showMessageRequestParams

  and showMessageRequestParams = {
    type_: MessageType.t;
    message: string;
    actions: messageActionItem list;
  }

  and messageActionItem = {
    title: string;
  }
end


(* Progress notification, method="window/progress" *)
module Progress = struct
  type t = Some of {id: int; label: string;} | None

  and params = progressParams

  and progressParams = {
    (* LSP progress notifications have a lifetime that starts with their 1st  *)
    (* window/progress update message and ends with an update message with    *)
    (* label = None. They use an ID number (not JsonRPC id) to associate      *)
    (* multiple messages to a single lifetime stream.                         *)
    id: int;
    label: string option;
  }
end


(* ActionRequired notification, method="window/actionRequired" *)
module ActionRequired = struct
  type t = Some of {id: int; label: string;} | None

  and params = actionRequiredParams

  and actionRequiredParams = {
    (* See progressParams.id for an explanation of this field. *)
    id: int;
    label: string option;
  }
end


(* ErrorResponse *)
module Error = struct
  (* Defined by JSON-RPC. *)
  exception Parse of string (* -32700 *)
  exception InvalidRequest of string (* -32600 *)
  exception MethodNotFound of string (* -32601 *)
  exception InvalidParams of string (* -32602 *)
  exception InternalError of string (* -32603 *)
  exception ServerErrorStart of string * Initialize.errorData (* -32099 *)
  exception ServerErrorEnd of string (* -32000 *)
  exception ServerNotInitialized of string (* -32002 *)
  exception Unknown of string (* -32001 *)

  (* Defined by the protocol. *)
  exception RequestCancelled of string (* -32800 *)
end
