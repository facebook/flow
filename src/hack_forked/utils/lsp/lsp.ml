(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module UriSet = Flow_set.Make (DocumentUri)
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

let flow_position_to_lsp (line : int) (char : int) : position =
  { line = max 0 (line - 1); character = char }

let lsp_position_to_flow (position : position) : int * int =
  (* Flow's line numbers are 1-indexed; LSP's are 0-indexed *)
  let line = position.line + 1 in
  let char = position.character in
  (line, char)

let lsp_position_to_flow_position p =
  let (line, column) = lsp_position_to_flow p in
  { Loc.line; column }

let lsp_range_to_flow_loc ?source (range : range) =
  {
    Loc.source;
    start = lsp_position_to_flow_position range.start;
    _end = lsp_position_to_flow_position range.end_;
  }

let loc_to_lsp_range (loc : Loc.t) : range =
  let loc_start = loc.Loc.start in
  let loc_end = loc.Loc._end in
  let start = flow_position_to_lsp loc_start.Loc.line loc_start.Loc.column in
  let end_ = flow_position_to_lsp loc_end.Loc.line loc_end.Loc.column in
  { start; end_ }

(** Represents a location inside a resource, such as a line inside a text file *)
module Location = struct
  type t = {
    uri: DocumentUri.t;
    range: range;
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

(** A special text edit to provide an insert and a replace operation. *)
module InsertReplaceEdit = struct
  type t = {
    newText: string;  (** The string to be inserted. *)
    insert: range;  (** The range if the insert is requested *)
    replace: range;  (** The range if the replace is requested. *)
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
    selectionRange: range;
    containerName: string option;  (** the symbol containing this symbol *)
  }
end

module WorkspaceSymbolInformation = struct
  type t = {
    name: string;
    kind: SymbolInformation.symbolKind;
    location: TextDocumentIdentifier.t;
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

  (** [is_kind k x] determines if [x] is of kind [k] ([k] is a prefix of [x]) *)
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

  (* Some categories are handled specially in VS Code:
     https://github.com/microsoft/vscode/blob/7c19b93062ae9990c1c9b3bf9e792f55a50db83a/src/vs/editor/contrib/codeAction/common/types.ts#L17-L26 *)

  (** Some of the constants defined by the spec *)
  let quickfix = kind_of_string "quickfix"

  let refactor = kind_of_string "refactor"

  let refactor_extract = sub_kind refactor "extract"

  let refactor_inline = sub_kind refactor "inline"

  let refactor_move = sub_kind refactor "move"

  let refactor_rewrite = sub_kind refactor "rewrite"

  (** Document wide code actions *)
  let source = kind_of_string "source"

  let source_organize_imports = sub_kind source "organizeImports"

  let source_fix_all = sub_kind source "fixAll"
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

module CompletionItemTag = struct
  type t = Deprecated [@value 1] [@@deriving enum, eq, show]
end

module CompletionClientCapabilities = struct
  type tagSupport = { valueSet: CompletionItemTag.t list }

  (** The client supports the following `CompletionItem` specific capabilities. *)
  type completionItem = {
    snippetSupport: bool;  (** client can do snippets as insert text *)
    preselectSupport: bool;  (** client supports the preselect property *)
    tagSupport: tagSupport;  (** client supports the tags property *)
    insertReplaceSupport: bool;
        (** Client supports insert replace edit to control different behavior if
            a completion item is inserted in the text or should replace text. *)
    labelDetailsSupport: bool;  (** proposed for 3.17 *)
  }

  type t = { completionItem: completionItem }
end

module DocumentSymbolClientCapabilities = struct
  (** Specific capabilities for the `SymbolKind` in the
      `textDocument/documentSymbol` request. *)
  type symbolKind = {
    valueSet: SymbolInformation.symbolKind list option;
        (** The symbol kind values the client supports. When this
            property exists the client also guarantees that it will
            handle values outside its set gracefully and falls back
            to a default value when unknown.

            If this property is not present the client only supports
            the symbol kinds from `File` to `Array` as defined in
            the initial version of the protocol. *)
  }

  type t = {
    symbolKind: symbolKind;
    hierarchicalDocumentSymbolSupport: bool;
        (** The client supports hierarchical document symbols. *)
  }
end

module TextDocumentSyncClientCapabilities = struct
  (** synchronization capabilities say what messages the client is capable
      of sending, should be be so asked by the server. *)
  type t = {
    willSave: bool;  (** The client supports sending will save notifications. *)
    willSaveWaitUntil: bool;
        (** The client supports sending a will save request and
	          waits for a response providing text edits which will
	          be applied to the document before it is saved. *)
    didSave: bool;  (** The client supports did save notifications. *)
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

module DiagnosticTag = struct
  type t =
    | Unnecessary [@value 1]
    | Deprecated [@value 2]
  [@@deriving enum]
end

module PublishDiagnosticsClientCapabilities = struct
  type t = {
    relatedInformation: bool;
    tagSupport: tagSupport;
    versionSupport: bool;
    codeDescriptionSupport: bool;
    dataSupport: bool;
  }

  and tagSupport = { valueSet: DiagnosticTag.t list }
end

module LinkedEditingRangeClientCapabilities = struct
  type t = { dynamicRegistration: bool }
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

module TextDocumentSyncKind = struct
  type t =
    | NoSync [@value 0]  (** docs should not be synced at all. Wire "None" *)
    | FullSync [@value 1]  (** synced by always sending full content. Wire "Full" *)
    | IncrementalSync [@value 2]  (** full only on open. Wire "Incremental" *)
  [@@deriving enum]
end

module TextDocumentSyncOptions = struct
  (** text document sync options say what messages the server requests the
      client to send. *)
  type t = {
    openClose: bool;  (** textDocument/didOpen+didClose *)
    change: TextDocumentSyncKind.t;
    willSave: bool;  (** textDocument/willSave *)
    willSaveWaitUntil: bool;  (** textDoc.../willSaveWaitUntil *)
    save: saveOptions option;  (** textDocument/didSave *)
  }

  and saveOptions = { includeText: bool  (** the client should include content on save *) }
end

module FileOperationOptions = struct
  type fileOperationsServerCapabilities = { willRename: fileOperationRegistrationOptions option }

  and fileOperationRegistrationOptions = { filters: fileOperationFilter list }

  and fileOperationFilter = {
    pattern: fileOperationPattern;
    scheme: string option;
  }

  and fileOperationPattern = {
    glob: string;
    matches: fileOperationPatternKind;
    options: fileOperationPatternOptions;
  }

  and fileOperationPatternKind =
    | File
    | Folder

  and fileOperationPatternOptions = { ignoreCase: bool }
end

(** Initialize request, method="initialize" *)
module Initialize = struct
  type params = {
    processId: int option;  (** pid of parent process *)
    rootPath: string option;  (** deprecated *)
    rootUri: DocumentUri.t option;  (** the root URI of the workspace *)
    initializationOptions: initializationOptions;
    client_capabilities: client_capabilities;  (** "capabilities" over wire *)
    trace: trace;  (** the initial trace setting, default="off" *)
  }

  and result = {
    server_capabilities: server_capabilities;  (** "capabilities" over wire *)
    server_info: serverInfo;  (** name and version of the language server; "serverInfo" over wire *)
  }

  and errorData = { retry: bool  (** should client retry the initialize request *) }

  and trace =
    | Off
    | Messages
    | Verbose

  and initializationOptions = {
    liveSyntaxErrors: bool;
    detailedErrorRendering: bool option;
    semanticDecorations: bool;
    refinementInformationOnHover: bool;
  }

  and client_capabilities = {
    workspace: workspaceClientCapabilities;
    textDocument: textDocumentClientCapabilities;
    window: windowClientCapabilities;
    telemetry: telemetryClientCapabilities;
    experimental: experimentalClientCapabilities;
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

  (** omitted: dynamic-registration fields *)
  and textDocumentClientCapabilities = {
    synchronization: TextDocumentSyncClientCapabilities.t;
    completion: CompletionClientCapabilities.t;
    codeAction: CodeActionClientCapabilities.t;
    documentSymbol: DocumentSymbolClientCapabilities.t;
    signatureHelp: SignatureHelpClientCapabilities.t;
    selectionRange: SelectionRangeClientCapabilities.t;
    publishDiagnostics: PublishDiagnosticsClientCapabilities.t;
    linkedEditingRange: LinkedEditingRangeClientCapabilities.t;
  }

  and windowClientCapabilities = {
    status: bool;  (** Nuclide-specific: client supports window/showStatusRequest *)
  }

  and telemetryClientCapabilities = {
    connectionStatus: bool;  (** Nuclide-specific: client supports telemetry/connectionStatus *)
  }

  (* Flow LSP specific capabilities. *)
  and experimentalClientCapabilities = { snippetTextEdit: bool }

  and experimentalServerCapabilities = {
    server_snippetTextEdit: bool;
    pasteProvider: bool;  (** Flow specific before LSP integration is available *)
    strictCompletionOrder: bool;
        (** true if the server strictly orders completion results. when set, the editor
            should not do its own sorting. *)
    autoCloseJsx: bool;
    renameFileImports: bool;
  }

  and workspaceServerCapabilities = {
    fileOperations: FileOperationOptions.fileOperationsServerCapabilities;
  }

  (** What capabilities the server provides *)
  and server_capabilities = {
    textDocumentSync: TextDocumentSyncOptions.t;  (** how to sync *)
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
    renameProvider: renameOptions option;
    documentLinkProvider: documentLinkOptions option;
    executeCommandProvider: executeCommandOptions option;
    implementationProvider: bool;
    selectionRangeProvider: bool;
    server_experimental: experimentalServerCapabilities;
    typeCoverageProvider: bool;  (** nuclide-specific *)
    rageProvider: bool;  (** nuclide-specific *)
    linkedEditingRangeProvider: bool;
    server_workspace: workspaceServerCapabilities;
  }

  and serverInfo = {
    name: string;
    version: string;
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

  and renameOptions = { prepareProvider: bool option }
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

module Ping = struct
  type result = { start_server_status: string option }
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
  module ExtraDetailedDiagnosticV0 = struct
    type raw_color =
      | Default
      | Black
      | Red
      | Green
      | Yellow
      | Blue
      | Magenta
      | Cyan
      | White

    type style =
      | Normal of raw_color
      | Bold of raw_color
      | Dim of raw_color
      | Underline of raw_color
      | BoldUnderline of raw_color
      | DimUnderline of raw_color
  end

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
    tags: DiagnosticTag.t list;
    relatedInformation: diagnosticRelatedInformation list;
    data: data;
  }

  and diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  and diagnosticRelatedInformation = {
    relatedLocation: Location.t;  (** wire: just "location" *)
    relatedMessage: string;  (** wire: just "message" *)
  }

  and data =
    | NoExtraData
    | RefinementInformation
    | ExtraDetailedDiagnosticV0 of (ExtraDetailedDiagnosticV0.style * string) list
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

(** Go to Definition request, method="textDocument/definition" *)
module Definition = struct
  type params = TextDocumentPositionParams.t

  and result = Location.t list
end

(** Go to Type Definition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct
  type params = TextDocumentPositionParams.t

  and result = Location.t list
end

(** The workspace/applyEdit request is sent from the server to the client to modify
    resource on the client side. *)
module ApplyWorkspaceEdit = struct
  type params = {
    label: string option;
        (** An optional label of the workspace edit. This label is
	          presented in the user interface for example on an undo
	          stack to undo the workspace edit. *)
    edit: WorkspaceEdit.t;  (** The edits to apply. *)
  }

  and result = {
    applied: bool;  (** Indicates whether the edit was applied or not. *)
    failureReason: string option;
        (** An optional textual description for why the edit was not applied.
            This may be used by the server for diagnostic logging or to provide
            a suitable error for a request that triggered the edit. *)
    failedChange: int option;
        (** Depending on the client's failure handling strategy `failedChange`
            might contain the index of the change that failed. This property is
            only available if the client signals a `failureHandling` strategy
            in its client capabilities. *)
  }
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
    description: string option;
    detail: string option;
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
  [@@deriving enum, eq, show]

  (* These numbers should match
   * https://microsoft.github.io/language-server-protocol/specification#textDocument_completion
   *)
  type insertTextFormat =
    | PlainText [@value 1]  (** the insertText/textEdits are just plain strings *)
    | SnippetFormat [@value 2]  (** wire: just "Snippet" *)
  [@@deriving enum, eq, show]

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
    (* extra annotations that tweak the rendering of a completion item. *)
    tags: CompletionItemTag.t list option;
    preselect: bool;  (** select this item when showing *)
    sortText: string option;  (** used for sorting; if absent, uses label *)
    filterText: string option;  (** used for filtering; if absent, uses label *)
    insertText: string option;  (** used for inserting; if absent, uses label *)
    insertTextFormat: insertTextFormat option;
    textEdit: [ `TextEdit of TextEdit.t | `InsertReplaceEdit of InsertReplaceEdit.t ] option;
    additionalTextEdits: TextEdit.t list;
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

(* Pull text document diagnostics, method="textDocument/diagnostic." *)
module TextDocumentDiagnostics = struct
  type params = { textDocument: TextDocumentIdentifier.t }

  and result = PublishDiagnostics.diagnostic list
end

(* Workspace Symbols request, method="workspace/symbol" *)
module WorkspaceSymbol = struct
  type params = workspaceSymbolParams

  and result =
    | SymbolInformation of SymbolInformation.t list
    | WorkspaceSymbolInformation of WorkspaceSymbolInformation.t list

  and workspaceSymbolParams = { query: string (* a non-empty query string *) }
end

(* Document Symbols request, method="textDocument/documentSymbol" *)
module DocumentSymbol = struct
  type t = {
    name: string;
    detail: string option;
    kind: SymbolInformation.symbolKind;
    deprecated: bool;
    range: range;
    selectionRange: range;
    children: t list option;
  }

  type params = { textDocument: TextDocumentIdentifier.t }

  type result =
    [ `SymbolInformation of SymbolInformation.t list
    | `DocumentSymbol of t list
    ]
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

(* Workspace Prepare Rename request, method="textDocument/prepareRename" *)
module PrepareRename = struct
  type params = TextDocumentPositionParams.t

  type result = range option
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
    backgroundColor: [ `error | `warning ] option;
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

(* Module for shared file renaming types *)
module RenameFiles = struct
  type fileRename = {
    oldUri: DocumentUri.t;
    newUri: DocumentUri.t;
  }
end

(* Module for will renamed files, method="workspace/willRenameFiles" *)
module WillRenameFiles = struct
  type params = { files: RenameFiles.fileRename list }

  and result = WorkspaceEdit.t
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

(** Auto close jsx tag request, method="flow/autoCloseJsx"
    This is a non-standard LSP extension. *)
module AutoCloseJsx = struct
  type params = TextDocumentPositionParams.t

  and result = string option
end

module DocumentPaste = struct
  type import_type =
    | ImportNamedValue
    | ImportValueAsNamespace
    | ImportNamedType
    | ImportNamedTypeOf
    | ImportTypeOfAsNamespace

  type import_item = {
    remote_name: string;
    local_name: string option;
    import_type: import_type;
    import_source: string;
    import_source_is_resolved: bool;
  }

  type data_transfer = ImportMetadata of { imports: import_item list }

  type prepare_params =
    | PrepareParams of {
        uri: DocumentUri.t;
        ranges: range list;
      }

  type provide_params =
    | ProvideParams of {
        text_document: TextDocumentItem.t;
        ranges: range list;
        data_transfer: data_transfer;
      }
end

(** LinkedEditingRange request, method="textDocument/linkedEditingRange" *)
module LinkedEditingRange = struct
  type params = TextDocumentPositionParams.t

  and result = linkedEditingRanges option

  and linkedEditingRanges = {
    ranges: range list;
    wordPattern: string option;
  }
end

(** Rename imports after a module rename, method="flow/renameFileImports"
    This is a non-standard LSP extension. *)
module RenameFileImports = struct
  type params = RenameFiles.fileRename

  and result = WorkspaceEdit.t
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
  | TextDocumentDiagnosticsRequest of TextDocumentDiagnostics.params
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
  | PingRequest
  | PrepareRenameRequest of PrepareRename.params
  | RenameRequest of Rename.params
  | DocumentCodeLensRequest of DocumentCodeLens.params
  | ExecuteCommandRequest of ExecuteCommand.params
  | ApplyWorkspaceEditRequest of ApplyWorkspaceEdit.params
  | WillRenameFilesRequest of WillRenameFiles.params
  | AutoCloseJsxRequest of AutoCloseJsx.params
  | PrepareDocumentPasteRequest of DocumentPaste.prepare_params
  | ProvideDocumentPasteRequest of DocumentPaste.provide_params
  | LinkedEditingRangeRequest of LinkedEditingRange.params
  | RenameFileImportsRequest of RenameFileImports.params
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
  | TextDocumentDiagnosticsResult of TextDocumentDiagnostics.result
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
  | PingResult of Ping.result
  | PrepareRenameResult of PrepareRename.result
  | RenameResult of Rename.result
  | DocumentCodeLensResult of DocumentCodeLens.result
  | ExecuteCommandResult of ExecuteCommand.result
  | ApplyWorkspaceEditResult of ApplyWorkspaceEdit.result
  | WillRenameFilesResult of WillRenameFiles.result
  | RegisterCapabilityResult
  | AutoCloseJsxResult of AutoCloseJsx.result
  | PrepareDocumentPasteResult of DocumentPaste.data_transfer
  | ProvideDocumentPasteResult of WorkspaceEdit.t
  | LinkedEditingRangeResult of LinkedEditingRange.result
  | RenameFileImportsResult of RenameFileImports.result
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
  | ApplyWorkspaceEditHandler of (ApplyWorkspaceEdit.result -> 'a -> 'a)
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

module IdSet = Flow_set.Make (IdKey)
module IdMap = WrappedMap.Make (IdKey)
