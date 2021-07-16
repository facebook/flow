(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

type lsp_id =
  | NumberId of int
  | StringId of string

module DocumentUri = struct
  type t = DocumentUri of string

  let compare (DocumentUri x) (DocumentUri y) = String.compare x y

  let of_string (s : string) : t = DocumentUri s

  let to_string (DocumentUri s) : string = s
end

module UriSet = Set.Make (DocumentUri)
module UriMap = WrappedMap.Make (DocumentUri)

(** A position is between two characters like an 'insert' cursor in a editor *)
type position = {
  line: int;  (** line position in a document [zero-based] *)
  character: int;  (** character offset on a line in a document [zero-based] *)
}

(** A range is comparable to a selection in an editor *)
type range = {
  start: position;  (** the range's start position *)
  end_: position;  (** the range's end position [exclusive] *)
}

(** Represents a location inside a resource, such as a line inside a text file *)
module Location = struct
  type t = {
    uri: DocumentUri.t;
    range: range;
  }
end

(** Represents a location inside a resource which also wants to display a
    friendly name to the user. *)
module DefinitionLocation = struct
  type t = {
    location: Location.t;
    title: string option;
  }
end

module MarkupKind = struct
  type t =
    | Markdown
    | PlainText
end

module MarkupContent = struct
  type t = {
    kind: MarkupKind.t;
    value: string;
  }
end

(** markedString can be used to render human readable text. It is either a
    markdown string or a code-block that provides a language and a code snippet.
    Note that markdown strings will be sanitized by the client - including
    escaping html *)
type markedString =
  | MarkedString of string
  | MarkedCode of string * string  (** lang, value *)

(** Represents a reference to a command. Provides a title which will be used to
    represent a command in the UI. Commands are identitifed using a string
    identifier and the protocol currently doesn't specify a set of well known
    commands. So executing a command requires some tool extension code. *)
module Command = struct
  type name = Command of string

  type t = {
    title: string;  (** title of the command, like `save` *)
    command: name;  (** the identifier of the actual command handler *)
    arguments: Hh_json.json list;  (** wire: it can be omitted *)
  }
end

(** A textual edit applicable to a text document. If n textEdits are applied
    to a text document all text edits describe changes to the initial document
    version. Execution wise text edits should applied from the bottom to the
    top of the text document. Overlapping text edits are not supported. *)
module TextEdit = struct
  type t = {
    range: range;  (** to insert text, use a range where start = end *)
    newText: string;  (** for delete operations, use an empty string *)
  }
end

(** Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = { uri: DocumentUri.t  (** the text document's URI *) }
end

(** An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t = {
    uri: DocumentUri.t;  (** the text document's URI *)
    version: int;  (** the version number of this document *)
  }
end

(** Describes textual changes on a single text document. The text document is
    referred to as a VersionedTextDocumentIdentifier to allow clients to check
    the text document version before an edit is applied. *)
module TextDocumentEdit = struct
  type t = {
    textDocument: VersionedTextDocumentIdentifier.t;
    edits: TextEdit.t list;
  }
end

(** A workspace edit represents changes to many resources managed in the
    workspace. A workspace edit consists of a mapping from a URI to an
    array of TextEdits to be applied to the document with that URI. *)
module WorkspaceEdit = struct
  type t = { changes: TextEdit.t list UriMap.t  (** holds changes to existing docs *) }
end

(** An item to transfer a text document from the client to the server. The
    version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t = {
    uri: DocumentUri.t;  (** the text document's URI *)
    languageId: string;  (** the text document's language identifier *)
    version: int;  (** the version of the document *)
    text: string;  (** the content of the opened text document *)
  }
end

(**
 * A code lens represents a command that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For performance
 * reasons the creation of a code lens and resolving should be done in two stages.
 *)
module CodeLens = struct
  type t = {
    range: range;
    command: Command.t;
    data: Hh_json.json option;
  }
end

(** A parameter literal used in requests to pass a text document and a position
    inside that document. *)
module TextDocumentPositionParams = struct
  type t = {
    textDocument: TextDocumentIdentifier.t;  (** the text document *)
    position: position;  (** the position inside the text document *)
  }
end

(** A document filter denotes a document through properties like language,
    schema or pattern. E.g. language:"typescript",scheme:"file"
    or language:"json",pattern:"**/package.json" *)
module DocumentFilter = struct
  type t = {
    language: string option;  (** a language id, like "typescript" *)
    scheme: string option;  (** a uri scheme, like "file" or "untitled" *)
    pattern: string option;  (** a glob pattern, like "*.{ts,js}" *)
  }
end

(** A document selector is the combination of one or many document filters. *)
module DocumentSelector = struct
  type t = DocumentFilter.t list
end

(** Represents information about programming constructs like variables etc. *)
module SymbolInformation = struct
  (* These numbers should match
   * https://microsoft.github.io/language-server-protocol/specification#textDocument_documentSymbol
   *)
  type symbolKind =
    | File [@value 1]
    | Module [@value 2]
    | Namespace [@value 3]
    | Package [@value 4]
    | Class [@value 5]
    | Method [@value 6]
    | Property [@value 7]
    | Field [@value 8]
    | Constructor [@value 9]
    | Enum [@value 10]
    | Interface [@value 11]
    | Function [@value 12]
    | Variable [@value 13]
    | Constant [@value 14]
    | String [@value 15]
    | Number [@value 16]
    | Boolean [@value 17]
    | Array [@value 18]
    | Object [@value 19]
    | Key [@value 20]
    | Null [@value 21]
    | EnumMember [@value 22]
    | Struct [@value 23]
  [@@deriving enum]

  type t = {
    name: string;
    kind: symbolKind;
    location: Location.t;  (** the span of the symbol including its contents *)
    containerName: string option;  (** the symbol containing this symbol *)
  }
end

(** For showing messages (not diagnostics) in the user interface. *)
module MessageType = struct
  type t =
    | ErrorMessage [@value 1]
    | WarningMessage [@value 2]
    | InfoMessage [@value 3]
    | LogMessage [@value 4]
  [@@deriving enum]
end

module CodeActionKind = struct
  (** The kind of a code action.
      Kinds are a hierarchical list of identifiers separated by `.`, e.g.
      `"refactor.extract.function"`.
      The set of kinds is open and client needs to announce the kinds it supports
      to the server during initialization.
      CodeActionKind.t uses a pair to represent a non-empty list and provides utility
      functions for creation, membership, printing.
      Module CodeAction below also references this module as Kind.
   *)
  type t = string * string list

  (** is x of kind k? *)
  let is_kind : t -> t -> bool =
    let rec is_prefix_of ks xs =
      match (ks, xs) with
      | ([], _) -> true
      | (k :: ks, x :: xs) when String.equal k x -> is_prefix_of ks xs
      | (_, _) -> false
    in
    (fun (k, ks) (x, xs) -> String.equal k x && is_prefix_of ks xs)

  (** does `ks` contain kind `k` *)
  let contains_kind k ks = List.exists (is_kind k) ks

  (** does an optional list of kinds `ks` contain kind `k` *)
  let contains_kind_opt ~default k ks =
    match ks with
    | Some ks -> contains_kind k ks
    | None -> default

  (** Create a kind from a string that follows the spec *)
  let kind_of_string : string -> t =
   fun s ->
    match String.split_on_char '.' s with
    | [] -> failwith "split_on_char does not return an empty list"
    | k :: ks -> (k, ks)

  (** Create the equivalent string that the spec would have required *)
  let string_of_kind : t -> string = (fun (k, ks) -> String.concat "." (k :: ks))

  (* Create a new sub-kind of an existing kind *)
  let sub_kind : t -> string -> t =
    let cons_to_end (ss : string list) (s : string) = Base.List.(fold_right ss ~f:cons ~init:[s]) in
    (fun (k, ks) s -> (k, cons_to_end ks s))

  (** Some of the constants defined by the spec *)
  let quickfix = kind_of_string "quickfix"

  let refactor_extract = kind_of_string "refactor.extract"

  (** Document wide code actions *)
  let source = kind_of_string "source"
end

(** Cancellation notification, method="$/cancelRequest" *)
module CancelRequest = struct
  type params = cancelParams

  and cancelParams = { id: lsp_id  (** the request id to cancel *) }
end

module CodeActionClientCapabilities = struct
  module CodeActionLiteralSupport = struct
    type t = {
      valueSet: CodeActionKind.t list;
          (** The code action kind values the client supports. When this
              property exists the client also guarantees that it will
              handle values outside its set gracefully and falls back
              to a default value when unknown. *)
    }
  end

  type t = {
    dynamicRegistration: bool;
        (** Whether code action supports dynamic registration. (wire: dynamicRegistraction) *)
    codeActionLiteralSupport: CodeActionLiteralSupport.t option;
        (** The client support code action literals as a valid
            response of the `textDocument/codeAction` request. *)
  }
end

module SelectionRangeClientCapabilities = struct
  type t = { dynamicRegistration: bool }
end

module SignatureHelpClientCapabilities = struct
  type t = {
    dynamicRegistration: bool;
    signatureInformation: signatureInformation;
    contextSupport: bool;
  }

  and signatureInformation = {
    documentationFormat: MarkupKind.t list;
    parameterInformation: parameterInformation;
  }

  and parameterInformation = { labelOffsetSupport: bool }
end

module CompletionOptions = struct
  type completionItem = {
    labelDetailsSupport: bool;
        (** The server has support for completion item label details (see also
            `CompletionItemLabelDetails`) when receiving a completion item in
            a resolve call.

            @since 3.17.0 - proposed state *)
  }

  type t = {
    resolveProvider: bool;  (** server resolves extra info on demand *)
    triggerCharacters: string list;
    completionItem: completionItem;
  }
end

(** Initialize request, method="initialize" *)
module Initialize = struct
  type textDocumentSyncKind =
    | NoSync [@value 0]  (** docs should not be synced at all. Wire "None" *)
    | FullSync [@value 1]  (** synced by always sending full content. Wire "Full" *)
    | IncrementalSync [@value 2]
  [@@deriving enum]

  type params = {
    processId: int option;  (** pid of parent process *)
    rootPath: string option;  (** deprecated *)
    rootUri: DocumentUri.t option;  (** the root URI of the workspace *)
    initializationOptions: initializationOptions;
    client_capabilities: client_capabilities;  (** "capabilities" over wire *)
    trace: trace;  (** the initial trace setting, default="off" *)
  }

  and result = { server_capabilities: server_capabilities  (** "capabilities" over wire *) }

  and errorData = { retry: bool  (** should client retry the initialize request *) }

  and trace =
    | Off
    | Messages
    | Verbose

  and initializationOptions = { liveSyntaxErrors: bool }

  and client_capabilities = {
    workspace: workspaceClientCapabilities;
    textDocument: textDocumentClientCapabilities;
    window: windowClientCapabilities;
    telemetry: telemetryClientCapabilities; (* omitted: experimental *)
  }

  and workspaceClientCapabilities = {
    applyEdit: bool;  (** client supports appling batch edits *)
    configuration: bool;  (** client supports workspace/configuration requests *)
    workspaceEdit: workspaceEdit;
    didChangeConfiguration: dynamicRegistration;
        (** client supports workspace/didChangeConfiguration notifications *)
    didChangeWatchedFiles: dynamicRegistration; (* omitted: other dynamic-registration fields *)
  }

  and dynamicRegistration = {
    dynamicRegistration: bool;  (** client supports dynamic registration for this capability *)
  }

  and workspaceEdit = { documentChanges: bool  (** client supports versioned doc changes *) }

  and textDocumentClientCapabilities = {
    synchronization: synchronization;
    completion: completion;  (** textDocument/completion *)
    codeAction: CodeActionClientCapabilities.t;
    (* omitted: dynamic-registration fields *)
    signatureHelp: SignatureHelpClientCapabilities.t;
    selectionRange: SelectionRangeClientCapabilities.t;
  }

  (** synchronization capabilities say what messages the client is capable
      of sending, should be be so asked by the server.
      We use the "can_" prefix for OCaml naming reasons; it's absent in LSP *)
  and synchronization = {
    can_willSave: bool;  (** client can send textDocument/willSave *)
    can_willSaveWaitUntil: bool;  (** textDoc.../willSaveWaitUntil *)
    can_didSave: bool;  (** textDocument/didSave *)
  }

  and completion = { completionItem: completionItem }

  and completionItem = {
    snippetSupport: bool;  (** client can do snippets as insert text *)
    preselectSupport: bool;  (** client supports the preselect property *)
    labelDetailsSupport: bool;  (** proposed for 3.17 *)
  }

  and windowClientCapabilities = {
    status: bool;  (** Nuclide-specific: client supports window/showStatusRequest *)
  }

  and telemetryClientCapabilities = {
    connectionStatus: bool;  (** Nuclide-specific: client supports telemetry/connectionStatus *)
  }

  (** What capabilities the server provides *)
  and server_capabilities = {
    textDocumentSync: textDocumentSyncOptions;  (** how to sync *)
    hoverProvider: bool;
    completionProvider: CompletionOptions.t option;
    signatureHelpProvider: signatureHelpOptions option;
    definitionProvider: bool;
    typeDefinitionProvider: bool;
    referencesProvider: bool;
    documentHighlightProvider: bool;
    documentSymbolProvider: bool;  (** ie. document outline *)
    workspaceSymbolProvider: bool;  (** ie. find-symbol-in-project *)
    codeActionProvider: codeActionOptions;
    codeLensProvider: codeLensOptions option;
    documentFormattingProvider: bool;
    documentRangeFormattingProvider: bool;
    documentOnTypeFormattingProvider: documentOnTypeFormattingOptions option;
    renameProvider: bool;
    documentLinkProvider: documentLinkOptions option;
    executeCommandProvider: executeCommandOptions option;
    implementationProvider: bool;
    selectionRangeProvider: bool;
    typeCoverageProvider: bool;  (** nuclide-specific *)
    rageProvider: bool;  (** nuclide-specific *)
  }

  and signatureHelpOptions = {
    sighelp_triggerCharacters: string list;  (** wire "triggerCharacters" *)
  }

  and codeActionOptions =
    | CodeActionBool of bool
    | CodeActionOptions of { codeActionKinds: CodeActionKind.t list }

  and codeLensOptions = { codelens_resolveProvider: bool  (** wire "resolveProvider" *) }

  and documentOnTypeFormattingOptions = {
    firstTriggerCharacter: string;
    (* e.g. "}" *)
    moreTriggerCharacter: string list;
  }

  and documentLinkOptions = { doclink_resolveProvider: bool  (** wire "resolveProvider" *) }

  and executeCommandOptions = {
    commands: Command.name list;  (** the commands to be executed on the server *)
  }

  (** text document sync options say what messages the server requests the
      client to send. We use the "want_" prefix for OCaml naming reasons;
      this prefix is absent in LSP. *)
  and textDocumentSyncOptions = {
    want_openClose: bool;
    (* textDocument/didOpen+didClose *)
    want_change: textDocumentSyncKind;
    want_willSave: bool;
    (* textDocument/willSave *)
    want_willSaveWaitUntil: bool;
    (* textDoc.../willSaveWaitUntil *)
    want_didSave: saveOptions option; (* textDocument/didSave *)
  }

  (* full only on open. Wire "Incremental" *)
  and saveOptions = { includeText: bool  (** the client should include content on save *) }
end

(** Shutdown request, method="shutdown" *)
module Shutdown = struct end

(** Exit notification, method="exit" *)
module Exit = struct end

(** Rage request, method="telemetry/rage" *)
module Rage = struct
  type result = rageItem list

  and rageItem = {
    title: string option;
    data: string;
  }
end

(** Code Lens resolve request, method="codeLens/resolve" *)
module CodeLensResolve = struct
  type params = CodeLens.t

  and result = CodeLens.t
end

(** Hover request, method="textDocument/hover" *)
module Hover = struct
  type params = TextDocumentPositionParams.t

  and result = hoverResult option

  and hoverResult = {
    contents: markedString list;  (** wire: either a single one or an array *)
    range: range option;
  }
end

(** PublishDiagnostics notification, method="textDocument/PublishDiagnostics" *)
module PublishDiagnostics = struct
  type diagnosticSeverity =
    | Error [@value 1]
    | Warning [@value 2]
    | Information [@value 3]
    | Hint [@value 4]
  [@@deriving enum]

  type params = publishDiagnosticsParams

  and publishDiagnosticsParams = {
    uri: DocumentUri.t;
    diagnostics: diagnostic list;
  }

  and diagnostic = {
    range: range;  (** the range at which the message applies *)
    severity: diagnosticSeverity option;  (** if omitted, client decides *)
    code: diagnosticCode;  (** the diagnostic's code. *)
    source: string option;  (** human-readable string, eg. typescript/lint *)
    message: string;  (** the diagnostic's message *)
    relatedInformation: diagnosticRelatedInformation list;
    relatedLocations: relatedLocation list;  (** legacy FB extension *)
  }

  and diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  and diagnosticRelatedInformation = {
    relatedLocation: Location.t;  (** wire: just "location" *)
    relatedMessage: string;  (** wire: just "message" *)
  }

  (* legacy FB extension *)
  and relatedLocation = diagnosticRelatedInformation
end

(** DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = {
    textDocument: TextDocumentItem.t;  (** the document that was opened *)
  }
end

(** DidCloseTextDocument notification, method="textDocument/didClose" *)
module DidClose = struct
  type params = didCloseTextDocumentParams

  and didCloseTextDocumentParams = {
    textDocument: TextDocumentIdentifier.t;  (** the doc that was closed *)
  }
end

(** DidSaveTextDocument notification, method="textDocument/didSave" *)
module DidSave = struct
  type params = didSaveTextDocumentParams

  and didSaveTextDocumentParams = {
    textDocument: TextDocumentIdentifier.t;  (** the doc that was saved *)
    text: string option;  (** content when saved; depends on includeText *)
  }
end

(** DidChangeTextDocument notification, method="textDocument/didChange" *)
module DidChange = struct
  type params = didChangeTextDocumentParams

  and didChangeTextDocumentParams = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanges: textDocumentContentChangeEvent list;
  }

  and textDocumentContentChangeEvent = {
    range: range option;  (** the range of the document that changed *)
    rangeLength: int option;  (** the length that got replaced *)
    text: string;  (** the new text of the range/document *)
  }
end

(** Configuration changed notification, method="workspace/didChangeConfiguration" *)
module DidChangeConfiguration = struct
  type params = { settings: Hh_json.json }
end

(** Watched files changed notification, method="workspace/didChangeWatchedFiles" *)
module DidChangeWatchedFiles = struct
  type registerOptions = { watchers: fileSystemWatcher list }

  and fileSystemWatcher = { globPattern: string }

  type fileChangeType =
    | Created [@value 1]
    | Updated [@value 2]
    | Deleted [@value 3]
  [@@deriving enum]

  type params = { changes: fileEvent list }

  and fileEvent = {
    uri: DocumentUri.t;
    type_: fileChangeType;
  }
end

(** Goto Definition request, method="textDocument/definition" *)
module Definition = struct
  type params = TextDocumentPositionParams.t

  and result = DefinitionLocation.t list

  (* wire: either a single one or an array *)
end

(** Goto TypeDefinition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct
  type params = TextDocumentPositionParams.t

  and result = DefinitionLocation.t list
end

(** A code action represents a change that can be performed in code, e.g. to fix a problem or
    to refactor code. *)
module CodeAction = struct
  type t = {
    title: string;  (** A short, human-readable, title for this code action. *)
    kind: CodeActionKind.t;  (** The kind of the code action. Used to filter code actions. *)
    diagnostics: PublishDiagnostics.diagnostic list;
        (** The diagnostics that this code action resolves. *)
    action: edit_and_or_command;
        (** A CodeAction must set either `edit` and/or a `command`.
            If both are supplied, the `edit` is applied first, then the `command` is executed. *)
  }

  and edit_and_or_command =
    | EditOnly of WorkspaceEdit.t
    | CommandOnly of Command.t
    | BothEditThenCommand of (WorkspaceEdit.t * Command.t)

  type result = command_or_action list

  and command_or_action =
    | Command of Command.t
    | Action of t
end

(* Code Action Request, method="textDocument/codeAction" *)
module CodeActionRequest = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;  (** The document in which the command was invoked. *)
    range: range;  (** The range for which the command was invoked. *)
    context: codeActionContext;  (** Context carrying additional information. *)
  }

  (* Contains additional diagnostic information about the context in which
     a code action is run. *)
  and codeActionContext = {
    diagnostics: PublishDiagnostics.diagnostic list;
    only: CodeActionKind.t list option;
  }
end

(** proposed for 3.17: https://github.com/microsoft/vscode/issues/39441 *)
module CompletionItemLabelDetails = struct
  type t = {
    parameters: string option;
    qualifier: string option;
    type_: string option;
  }
end

(* Completion request, method="textDocument/completion" *)
module Completion = struct
  (* These numbers should match
   * https://microsoft.github.io/language-server-protocol/specification#textDocument_completion
   *)
  type completionItemKind =
    | Text [@value 1]
    | Method [@value 2]
    | Function [@value 3]
    | Constructor [@value 4]
    | Field [@value 5]
    | Variable [@value 6]
    | Class [@value 7]
    | Interface [@value 8]
    | Module [@value 9]
    | Property [@value 10]
    | Unit [@value 11]
    | Value [@value 12]
    | Enum [@value 13]
    | Keyword [@value 14]
    | Snippet [@value 15]
    | Color [@value 16]
    | File [@value 17]
    | Reference [@value 18]
    | Folder [@value 19]
    | EnumMember [@value 20]
    | Constant [@value 21]
    | Struct [@value 22]
    | Event [@value 23]
    | Operator [@value 24]
    | TypeParameter [@value 25]
  [@@deriving enum]

  (* These numbers should match
   * https://microsoft.github.io/language-server-protocol/specification#textDocument_completion
   *)
  type insertTextFormat =
    | PlainText [@value 1]  (** the insertText/textEdits are just plain strings *)
    | SnippetFormat [@value 2]  (** wire: just "Snippet" *)
  [@@deriving enum]

  type completionTriggerKind =
    | Invoked [@value 1]
    | TriggerCharacter [@value 2]
    | TriggerForIncompleteCompletions [@value 3]
  [@@deriving enum]

  type params = completionParams

  and completionParams = {
    loc: TextDocumentPositionParams.t;
    context: completionContext option;
  }

  and completionContext = {
    triggerKind: completionTriggerKind;
    triggerCharacter: string option;
  }

  and result = completionList

  (* wire: can also be 'completionItem list' *)
  and completionList = {
    isIncomplete: bool;
    (* further typing should result in recomputing *)
    items: completionItem list;
  }

  and completionItem = {
    label: string;  (** the label in the UI *)
    labelDetails: CompletionItemLabelDetails.t option;  (** proposed for 3.17 *)
    kind: completionItemKind option;  (** tells editor which icon to use *)
    detail: string option;  (** human-readable string like type/symbol info *)
    documentation: markedString list option;  (** human-readable doc-comment *)
    preselect: bool;  (** select this item when showing *)
    sortText: string option;  (** used for sorting; if absent, uses label *)
    filterText: string option;  (** used for filtering; if absent, uses label *)
    insertText: string option;  (** used for inserting; if absent, uses label *)
    insertTextFormat: insertTextFormat option;
    textEdits: TextEdit.t list;  (** wire: split into hd and tl *)
    command: Command.t option;  (** if present, is executed after completion *)
    data: Hh_json.json option;
  }
end

(* Completion Item Resolve request, method="completionItem/resolve" *)
module CompletionItemResolve = struct
  type params = Completion.completionItem

  and result = Completion.completionItem
end

(* Configuration request, method="workspace/configuration" *)
module Configuration = struct
  type params = { items: item list }

  and item = {
    scope_uri: DocumentUri.t option;
    section: string option;
  }

  and result = Hh_json.json list
end

(* Workspace Symbols request, method="workspace/symbol" *)
module WorkspaceSymbol = struct
  type params = workspaceSymbolParams

  and result = SymbolInformation.t list

  and workspaceSymbolParams = { query: string (* a non-empty query string *) }
end

(* Document Symbols request, method="textDocument/documentSymbol" *)
module DocumentSymbol = struct
  type params = documentSymbolParams

  and result = SymbolInformation.t list

  and documentSymbolParams = { textDocument: TextDocumentIdentifier.t }
end

(* Find References request, method="textDocument/references" *)
module FindReferences = struct
  type params = referenceParams

  and result = Location.t list

  and referenceParams = {
    loc: TextDocumentPositionParams.t;
    (* wire: loc's members are part of referenceParams *)
    context: referenceContext;
  }

  and referenceContext = {
    includeDeclaration: bool;
    (* include declaration of current symbol *)
    includeIndirectReferences: bool;
  }
end

(* Go To Implementation request, method="textDocument/implementation" *)
module GoToImplementation = struct
  type params = implementationParams

  and result = Location.t list

  and implementationParams = { loc: TextDocumentPositionParams.t }
end

(* Document Highlights request, method="textDocument/documentHighlight" *)
module DocumentHighlight = struct
  type params = TextDocumentPositionParams.t

  type documentHighlightKind =
    (* a textual occurrence *)
    | Text [@value 1]
    (* read-access of a symbol, like reading a variable *)
    | Read [@value 2]
    (* write-access of a symbol, like writing a variable *)
    | Write [@value 3]
  [@@deriving enum]

  type result = documentHighlight list

  and documentHighlight = {
    range: range;
    (* the range this highlight applies to *)
    kind: documentHighlightKind option;
  }
end

(* Type Coverage request, method="textDocument/typeCoverage" *)
(* THIS IS A NUCLIDE-SPECIFIC EXTENSION TO LSP.              *)
module TypeCoverage = struct
  type params = typeCoverageParams

  and result = {
    coveredPercent: int;
    uncoveredRanges: uncoveredRange list;
    defaultMessage: string;
  }

  and typeCoverageParams = { textDocument: TextDocumentIdentifier.t }

  and uncoveredRange = {
    range: range;
    message: string option;
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
    tabSize: int;
    (* size of a tab in spaces *)
    insertSpaces: bool;
        (* prefer spaces over tabs *)
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
    position: position;
    (* the position at which this request was sent *)
    ch: string;
    (* the character that has been typed *)
    options: DocumentFormatting.formattingOptions;
  }
end

(** Selection Range request, method="textDocument/selectionRange" *)
module SelectionRange = struct
  type params = {
    textDocument: TextDocumentIdentifier.t;
    positions: position list;
  }

  type selection_range = {
    range: range;
    parent: selection_range option;
  }

  type result = selection_range list
end

(* Document Signature Help request, method="textDocument/signatureHelp" *)
module SignatureHelp = struct
  module TriggerKind = struct
    type t =
      | Invoked [@value 1]
      | TriggerCharacter [@value 2]
      | ContentChange [@value 3]
    [@@deriving enum]
  end

  module Documentation = struct
    type t =
      | String of string
      | MarkupContent of MarkupContent.t
  end

  type params = {
    loc: TextDocumentPositionParams.t;
    context: context option;
  }

  and context = {
    triggerKind: TriggerKind.t;
    triggerCharacter: string option;
    isRetrigger: bool;
    activeSignatureHelp: result;
  }

  and result = t option

  and t = {
    signatures: signature_information list;
    activeSignature: int;
    activeParameter: int;
  }

  and signature_information = {
    siginfo_label: string;
    siginfo_documentation: Documentation.t option;
    parameters: parameter_information list;
  }

  and parameter_information = {
    parinfo_label: label;
    parinfo_documentation: Documentation.t option;
  }

  and label =
    | String of string
    | Offset of int * int
end

(* Workspace Rename request, method="textDocument/rename" *)
module Rename = struct
  type params = renameParams

  and result = WorkspaceEdit.t

  and renameParams = {
    textDocument: TextDocumentIdentifier.t;
    position: position;
    newName: string;
  }
end

(* Code Lens request, method="textDocument/codeLens" *)
module DocumentCodeLens = struct
  type params = codelensParams

  and result = CodeLens.t list

  and codelensParams = { textDocument: TextDocumentIdentifier.t }
end

(* Execute a command, method="workspace/executeCommand" *)
module ExecuteCommand = struct
  type params = executeCommandParams

  and result = unit

  and executeCommandParams = {
    command: Command.name;
    arguments: Hh_json.json list option;
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
  type t =
    | Present of { id: lsp_id }
    | Absent

  and params = showMessageRequestParams

  and result = messageActionItem option

  and showMessageRequestParams = {
    type_: MessageType.t;
    message: string;
    actions: messageActionItem list;
  }

  and messageActionItem = { title: string }
end

(* ShowStatus request, method="window/showStatus" *)
module ShowStatus = struct
  type params = showStatusParams

  and result = ShowMessageRequest.messageActionItem option

  and showStatusParams = {
    request: ShowMessageRequest.showMessageRequestParams;
    progress: int option;
    total: int option;
    shortMessage: string option;
  }
end

(* ConnectionStatus notification, method="telemetry/connectionStatus" *)
module ConnectionStatus = struct
  type params = connectionStatusParams

  and connectionStatusParams = { isConnected: bool }
end

(* Module for dynamic view, method="workspace/toggleTypeCoverage" *)
module ToggleTypeCoverage = struct
  type params = toggleTypeCoverageParams

  and toggleTypeCoverageParams = { toggle: bool }
end

(* ErrorResponse *)
module Error = struct
  type code =
    | ParseError [@value -32700]
    | InvalidRequest [@value -32600]
    | MethodNotFound [@value -32601]
    | InvalidParams [@value -32602]
    | InternalError [@value -32603]
    | ServerErrorStart [@value -32099]
    | ServerErrorEnd [@value -32000]
    | ServerNotInitialized [@value -32002]
    | UnknownErrorCode [@value -32001]
    | RequestCancelled [@value -32800]
    | ContentModified [@value -32801]
  [@@deriving show, enum]

  type t = {
    code: code;
    message: string;
    data: Hh_json.json option;
  }

  (* Legacy: some code uses exceptions instead of Error.t. *)
  (* Be careful with that since if you unmarshal one then you can't pattern-match it. *)

  exception LspException of t
end

(** Register capability request, method="client/registerCapability" *)
module RegisterCapability = struct
  type params = { registrations: registration list }

  and registration = {
    id: string;
    method_: string;
    registerOptions: options;
  }

  and options =
    | DidChangeConfiguration  (** has no options *)
    | DidChangeWatchedFiles of DidChangeWatchedFiles.registerOptions

  let make_registration (registerOptions : options) : registration =
    (* The ID field is arbitrary but unique per type of capability (for future
       deregistering, which we don't do). *)
    let (id, method_) =
      match registerOptions with
      | DidChangeConfiguration -> ("did-change-configuration", "workspace/didChangeConfiguration")
      | DidChangeWatchedFiles _ -> ("did-change-watched-files", "workspace/didChangeWatchedFiles")
    in
    { id; method_; registerOptions }
end

(**
 * Here are gathered-up ADTs for all the messages we handle
 *)

type lsp_request =
  | InitializeRequest of Initialize.params
  | RegisterCapabilityRequest of RegisterCapability.params
  | ShutdownRequest
  | CodeLensResolveRequest of CodeLensResolve.params
  | HoverRequest of Hover.params
  | DefinitionRequest of Definition.params
  | TypeDefinitionRequest of TypeDefinition.params
  | CodeActionRequest of CodeActionRequest.params
  | CompletionRequest of Completion.params
  | CompletionItemResolveRequest of CompletionItemResolve.params
  | ConfigurationRequest of Configuration.params
  | SelectionRangeRequest of SelectionRange.params
  | SignatureHelpRequest of SignatureHelp.params
  | WorkspaceSymbolRequest of WorkspaceSymbol.params
  | DocumentSymbolRequest of DocumentSymbol.params
  | FindReferencesRequest of FindReferences.params
  | DocumentHighlightRequest of DocumentHighlight.params
  | TypeCoverageRequest of TypeCoverage.params
  | DocumentFormattingRequest of DocumentFormatting.params
  | DocumentRangeFormattingRequest of DocumentRangeFormatting.params
  | DocumentOnTypeFormattingRequest of DocumentOnTypeFormatting.params
  | ShowMessageRequestRequest of ShowMessageRequest.params
  | ShowStatusRequest of ShowStatus.params
  | RageRequest
  | RenameRequest of Rename.params
  | DocumentCodeLensRequest of DocumentCodeLens.params
  | ExecuteCommandRequest of ExecuteCommand.params
  | UnknownRequest of string * Hh_json.json option

type lsp_result =
  | InitializeResult of Initialize.result
  | ShutdownResult
  | CodeLensResolveResult of CodeLensResolve.result
  | HoverResult of Hover.result
  | DefinitionResult of Definition.result
  | TypeDefinitionResult of TypeDefinition.result
  | CodeActionResult of CodeAction.result
  | CompletionResult of Completion.result
  | CompletionItemResolveResult of CompletionItemResolve.result
  | ConfigurationResult of Configuration.result
  | SelectionRangeResult of SelectionRange.result
  | SignatureHelpResult of SignatureHelp.result
  | WorkspaceSymbolResult of WorkspaceSymbol.result
  | DocumentSymbolResult of DocumentSymbol.result
  | FindReferencesResult of FindReferences.result
  | GoToImplementationResult of GoToImplementation.result
  | DocumentHighlightResult of DocumentHighlight.result
  | TypeCoverageResult of TypeCoverage.result
  | DocumentFormattingResult of DocumentFormatting.result
  | DocumentRangeFormattingResult of DocumentRangeFormatting.result
  | DocumentOnTypeFormattingResult of DocumentOnTypeFormatting.result
  | ShowMessageRequestResult of ShowMessageRequest.result
  | ShowStatusResult of ShowStatus.result
  | RageResult of Rage.result
  | RenameResult of Rename.result
  | DocumentCodeLensResult of DocumentCodeLens.result
  | ExecuteCommandResult of ExecuteCommand.result
  | RegisterCapabilityResult
  (* the string is a stacktrace *)
  | ErrorResult of Error.t * string

type lsp_notification =
  | ExitNotification
  | CancelRequestNotification of CancelRequest.params
  | PublishDiagnosticsNotification of PublishDiagnostics.params
  | DidOpenNotification of DidOpen.params
  | DidCloseNotification of DidClose.params
  | DidSaveNotification of DidSave.params
  | DidChangeNotification of DidChange.params
  | DidChangeConfigurationNotification of DidChangeConfiguration.params
  | DidChangeWatchedFilesNotification of DidChangeWatchedFiles.params
  | LogMessageNotification of LogMessage.params
  | TelemetryNotification of LogMessage.params (* LSP allows 'any' but we only send these *)
  | ShowMessageNotification of ShowMessage.params
  | ConnectionStatusNotification of ConnectionStatus.params
  | InitializedNotification
  | SetTraceNotification (* $/setTraceNotification *)
  | LogTraceNotification (* $/logTraceNotification *)
  | UnknownNotification of string * Hh_json.json option

type lsp_message =
  | RequestMessage of lsp_id * lsp_request
  | ResponseMessage of lsp_id * lsp_result
  | NotificationMessage of lsp_notification

type 'a lsp_handler = 'a lsp_result_handler * 'a lsp_error_handler

and 'a lsp_error_handler = Error.t * string -> 'a -> 'a

and 'a lsp_result_handler =
  | ShowMessageHandler of (ShowMessageRequest.result -> 'a -> 'a)
  | ShowStatusHandler of (ShowStatus.result -> 'a -> 'a)
  | ConfigurationHandler of (Configuration.result -> 'a -> 'a)
  | VoidHandler

module IdKey = struct
  type t = lsp_id

  let compare (x : t) (y : t) =
    match (x, y) with
    | (NumberId x, NumberId y) -> x - y
    | (NumberId _, StringId _) -> -1
    | (StringId x, StringId y) -> String.compare x y
    | (StringId _, NumberId _) -> 1
end

module IdSet = Set.Make (IdKey)
module IdMap = WrappedMap.Make (IdKey)
