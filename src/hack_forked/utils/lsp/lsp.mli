(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type lsp_id =
  | NumberId of int
  | StringId of string

module DocumentUri : sig
  (* Note: this datatype provides no invariants that the string is well-formed. *)
  type t = DocumentUri of string

  val compare : t -> t -> int

  val of_string : string -> t

  val to_string : t -> string
end

module UriSet : sig
  include module type of Flow_set.Make (DocumentUri)
end

module UriMap : sig
  include module type of WrappedMap.Make (DocumentUri)
end

type position = {
  line: int;
  character: int;
}

type range = {
  start: position;
  end_: position;
}

val flow_position_to_lsp : int -> int -> position

val lsp_position_to_flow : position -> int * int

val lsp_position_to_flow_position : position -> Loc.position

val lsp_range_to_flow_loc : ?source:File_key.t -> range -> Loc.t

val loc_to_lsp_range : Loc.t -> range

module Location : sig
  type t = {
    uri: DocumentUri.t;
    range: range;
  }
end

module MarkupKind : sig
  type t =
    | Markdown
    | PlainText
end

module MarkupContent : sig
  type t = {
    kind: MarkupKind.t;
    value: string;
  }
end

type markedString =
  | MarkedString of string
  | MarkedCode of string * string

module Command : sig
  type name = Command of string

  type t = {
    (* title of the command, like `save` *)
    title: string;
    (* the identifier of the actual command handler *)
    command: name;
    (* wire: it can be omitted *)
    arguments: Hh_json.json list;
  }
end

module TextEdit : sig
  type t = {
    range: range;
    newText: string;
  }
end

module InsertReplaceEdit : sig
  type t = {
    newText: string;
    insert: range;
    replace: range;
  }
end

module TextDocumentIdentifier : sig
  type t = { uri: DocumentUri.t }
end

module VersionedTextDocumentIdentifier : sig
  type t = {
    uri: DocumentUri.t;
    version: int;
  }
end

module TextDocumentEdit : sig
  type t = {
    textDocument: VersionedTextDocumentIdentifier.t;
    edits: TextEdit.t list;
  }
end

module WorkspaceEdit : sig
  type t = { changes: TextEdit.t list UriMap.t (* holds changes to existing docs *) }
end

module TextDocumentItem : sig
  type t = {
    uri: DocumentUri.t;
    languageId: string;
    version: int;
    text: string;
  }
end

module CodeLens : sig
  type t = {
    range: range;
    command: Command.t;
    data: Hh_json.json option;
  }
end

module TextDocumentPositionParams : sig
  type t = {
    textDocument: TextDocumentIdentifier.t;
    position: position;
  }
end

module DocumentFilter : sig
  type t = {
    language: string option;
    scheme: string option;
    pattern: string option;
  }
end

module DocumentSelector : sig
  type t = DocumentFilter.t list
end

module SymbolInformation : sig
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
    location: Location.t;
    selectionRange: range;
    containerName: string option;
  }
end

module WorkspaceSymbolInformation : sig
  type t = {
    name: string;
    kind: SymbolInformation.symbolKind;
    location: TextDocumentIdentifier.t;
    containerName: string option;  (** the symbol containing this symbol *)
  }
end

module MessageType : sig
  type t =
    | ErrorMessage [@value 1]
    | WarningMessage [@value 2]
    | InfoMessage [@value 3]
    | LogMessage [@value 4]
  [@@deriving enum]
end

module CancelRequest : sig
  type params = cancelParams

  and cancelParams = { id: lsp_id }
end

module CodeActionKind : sig
  type t = string * string list

  val is_kind : t -> t -> bool

  val contains_kind : t -> t list -> bool

  val contains_kind_opt : default:bool -> t -> t list option -> bool

  val kind_of_string : string -> t

  val string_of_kind : t -> string

  val sub_kind : t -> string -> t

  val quickfix : t

  val refactor : t

  val refactor_extract : t

  val refactor_inline : t

  val refactor_move : t

  val refactor_rewrite : t

  val source : t

  val source_organize_imports : t

  val source_fix_all : t
end

module CodeActionClientCapabilities : sig
  module CodeActionLiteralSupport : sig
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

module CompletionItemTag : sig
  type t = Deprecated [@value 1] [@@deriving enum, eq, show]
end

module CompletionClientCapabilities : sig
  type tagSupport = { valueSet: CompletionItemTag.t list }

  type completionItem = {
    snippetSupport: bool;
    preselectSupport: bool;
    tagSupport: tagSupport;
    insertReplaceSupport: bool;
    labelDetailsSupport: bool;
  }

  type t = { completionItem: completionItem }
end

module DocumentSymbolClientCapabilities : sig
  type symbolKind = { valueSet: SymbolInformation.symbolKind list option }

  type t = {
    symbolKind: symbolKind;
    hierarchicalDocumentSymbolSupport: bool;
  }
end

module TextDocumentSyncClientCapabilities : sig
  type t = {
    willSave: bool;
    willSaveWaitUntil: bool;
    didSave: bool;
  }
end

module SelectionRangeClientCapabilities : sig
  type t = { dynamicRegistration: bool }
end

module SignatureHelpClientCapabilities : sig
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

module DiagnosticTag : sig
  type t =
    | Unnecessary [@value 1]
    | Deprecated [@value 2]
  [@@deriving enum]
end

module PublishDiagnosticsClientCapabilities : sig
  type t = {
    relatedInformation: bool;
    tagSupport: tagSupport;
    versionSupport: bool;
    codeDescriptionSupport: bool;
    dataSupport: bool;
  }

  and tagSupport = { valueSet: DiagnosticTag.t list }
end

module LinkedEditingRangeClientCapabilities : sig
  type t = { dynamicRegistration: bool }
end

module CompletionOptions : sig
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

module TextDocumentSyncKind : sig
  type t =
    | NoSync [@value 0]
    | FullSync [@value 1]
    | IncrementalSync [@value 2]
  [@@deriving enum]
end

module TextDocumentSyncOptions : sig
  type t = {
    openClose: bool;
    change: TextDocumentSyncKind.t;
    willSave: bool;
    willSaveWaitUntil: bool;
    save: saveOptions option;
  }

  and saveOptions = { includeText: bool }
end

module FileOperationOptions : sig
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

module Initialize : sig
  type params = {
    processId: int option;
    rootPath: string option;
    rootUri: DocumentUri.t option;
    initializationOptions: initializationOptions;
    client_capabilities: client_capabilities;
    trace: trace;
  }

  and result = {
    server_capabilities: server_capabilities;  (** "capabilities" over wire *)
    server_info: serverInfo;  (** details about the language server *)
  }

  and errorData = { retry: bool }

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
    applyEdit: bool;
    configuration: bool;
    workspaceEdit: workspaceEdit;
    didChangeConfiguration: dynamicRegistration;
    didChangeWatchedFiles: dynamicRegistration;
  }

  and dynamicRegistration = { dynamicRegistration: bool }

  and workspaceEdit = { documentChanges: bool }

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

  and windowClientCapabilities = { status: bool }

  and telemetryClientCapabilities = { connectionStatus: bool }

  and experimentalClientCapabilities = { snippetTextEdit: bool }

  and experimentalServerCapabilities = {
    server_snippetTextEdit: bool;
    pasteProvider: bool;  (** Flow specific before LSP integration is available *)
    strictCompletionOrder: bool;
    autoCloseJsx: bool;
    renameFileImports: bool;
  }

  and workspaceServerCapabilities = {
    fileOperations: FileOperationOptions.fileOperationsServerCapabilities;
  }

  and server_capabilities = {
    textDocumentSync: TextDocumentSyncOptions.t;
    hoverProvider: bool;
    completionProvider: CompletionOptions.t option;
    signatureHelpProvider: signatureHelpOptions option;
    definitionProvider: bool;
    typeDefinitionProvider: bool;
    referencesProvider: bool;
    documentHighlightProvider: bool;
    documentSymbolProvider: bool;
    workspaceSymbolProvider: bool;
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
    typeCoverageProvider: bool;
    rageProvider: bool;
    linkedEditingRangeProvider: bool;
    server_workspace: workspaceServerCapabilities;
  }

  and serverInfo = {
    name: string;
    version: string;
  }

  and signatureHelpOptions = { sighelp_triggerCharacters: string list }

  and codeActionOptions =
    | CodeActionBool of bool
    | CodeActionOptions of { codeActionKinds: CodeActionKind.t list }

  and codeLensOptions = { codelens_resolveProvider: bool }

  and documentOnTypeFormattingOptions = {
    firstTriggerCharacter: string;
    moreTriggerCharacter: string list;
  }

  and documentLinkOptions = { doclink_resolveProvider: bool }

  and executeCommandOptions = { commands: Command.name list }

  and renameOptions = { prepareProvider: bool option }
end

module Shutdown : sig end

module Exit : sig end

module Rage : sig
  type result = rageItem list

  and rageItem = {
    title: string option;
    data: string;
  }
end

module Ping : sig
  type result = { start_server_status: string option }
end

module CodeLensResolve : sig
  type params = CodeLens.t

  and result = CodeLens.t
end

module Hover : sig
  type params = TextDocumentPositionParams.t

  and result = hoverResult option

  and hoverResult = {
    contents: markedString list;
    range: range option;
  }
end

module PublishDiagnostics : sig
  module ExtraDetailedDiagnosticV0 : sig
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

  type diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  type diagnosticSeverity =
    | Error
    | Warning
    | Information
    | Hint

  val min_diagnosticSeverity : int

  val max_diagnosticSeverity : int

  val diagnosticSeverity_to_enum : diagnosticSeverity -> int

  val diagnosticSeverity_of_enum : int -> diagnosticSeverity option

  type params = publishDiagnosticsParams

  and publishDiagnosticsParams = {
    uri: DocumentUri.t;
    diagnostics: diagnostic list;
  }

  and diagnostic = {
    range: range;
    severity: diagnosticSeverity option;
    code: diagnosticCode;
    source: string option;
    message: string;
    tags: DiagnosticTag.t list;
    relatedInformation: diagnosticRelatedInformation list;
    data: data;
  }

  and diagnosticRelatedInformation = {
    relatedLocation: Location.t;
    relatedMessage: string;
  }

  and data =
    | NoExtraData
    | RefinementInformation
    | ExtraDetailedDiagnosticV0 of (ExtraDetailedDiagnosticV0.style * string) list
end

module DidOpen : sig
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams = { textDocument: TextDocumentItem.t }
end

module DidClose : sig
  type params = didCloseTextDocumentParams

  and didCloseTextDocumentParams = { textDocument: TextDocumentIdentifier.t }
end

module DidSave : sig
  type params = didSaveTextDocumentParams

  and didSaveTextDocumentParams = {
    textDocument: TextDocumentIdentifier.t;
    text: string option;
  }
end

module DidChange : sig
  type params = didChangeTextDocumentParams

  and didChangeTextDocumentParams = {
    textDocument: VersionedTextDocumentIdentifier.t;
    contentChanges: textDocumentContentChangeEvent list;
  }

  and textDocumentContentChangeEvent = {
    range: range option;
    rangeLength: int option;
    text: string;
  }
end

module DidChangeConfiguration : sig
  type params = { settings: Hh_json.json }
end

module DidChangeWatchedFiles : sig
  type registerOptions = { watchers: fileSystemWatcher list }

  and fileSystemWatcher = { globPattern: string }

  type fileChangeType =
    | Created
    | Updated
    | Deleted
  [@@deriving enum]

  type params = { changes: fileEvent list }

  and fileEvent = {
    uri: DocumentUri.t;
    type_: fileChangeType;
  }
end

module Definition : sig
  type params = TextDocumentPositionParams.t

  and result = Location.t list
end

module TypeDefinition : sig
  type params = TextDocumentPositionParams.t

  and result = Location.t list
end

module ApplyWorkspaceEdit : sig
  type params = {
    label: string option;
    edit: WorkspaceEdit.t;
  }

  and result = {
    applied: bool;
    failureReason: string option;
    failedChange: int option;
  }
end

module CodeAction : sig
  type t = {
    title: string;
    kind: CodeActionKind.t;
    diagnostics: PublishDiagnostics.diagnostic list;
    action: edit_and_or_command;
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

module CodeActionRequest : sig
  type params = {
    textDocument: TextDocumentIdentifier.t;
    range: range;
    context: codeActionContext;
  }

  and codeActionContext = {
    diagnostics: PublishDiagnostics.diagnostic list;
    only: CodeActionKind.t list option;
  }
end

(** proposed for 3.17: https://github.com/microsoft/vscode/issues/39441 *)
module CompletionItemLabelDetails : sig
  type t = {
    description: string option;
    detail: string option;
  }
end

module Completion : sig
  type completionItemKind =
    | Text  (** 1 *)
    | Method  (** 2 *)
    | Function  (** 3 *)
    | Constructor  (** 4 *)
    | Field  (** 5 *)
    | Variable  (** 6 *)
    | Class  (** 7 *)
    | Interface  (** 8 *)
    | Module  (** 9 *)
    | Property  (** 10 *)
    | Unit  (** 11 *)
    | Value  (** 12 *)
    | Enum  (** 13 *)
    | Keyword  (** 14 *)
    | Snippet  (** 15 *)
    | Color  (** 16 *)
    | File  (** 17 *)
    | Reference  (** 18 *)
    | Folder  (** 19 *)
    | EnumMember  (** 20 *)
    | Constant  (** 21 *)
    | Struct  (** 22 *)
    | Event  (** 23 *)
    | Operator  (** 24 *)
    | TypeParameter  (** 25 *)
  [@@deriving enum, eq, show]

  type insertTextFormat =
    | PlainText  (** 1 -- the insertText/textEdits are just plain strings *)
    | SnippetFormat  (** 2 -- wire: just "Snippet" *)
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

module CompletionItemResolve : sig
  type params = Completion.completionItem

  and result = Completion.completionItem
end

module Configuration : sig
  type params = { items: item list }

  and item = {
    scope_uri: DocumentUri.t option;
    section: string option;
  }

  and result = Hh_json.json list
end

module TextDocumentDiagnostics : sig
  type params = { textDocument: TextDocumentIdentifier.t }

  and result = PublishDiagnostics.diagnostic list
end

module WorkspaceSymbol : sig
  type params = workspaceSymbolParams

  and result =
    | SymbolInformation of SymbolInformation.t list
    | WorkspaceSymbolInformation of WorkspaceSymbolInformation.t list

  and workspaceSymbolParams = { query: string }
end

module DocumentSymbol : sig
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

module FindReferences : sig
  type params = referenceParams

  and result = Location.t list

  and referenceParams = {
    loc: TextDocumentPositionParams.t;
    context: referenceContext;
  }

  and referenceContext = {
    includeDeclaration: bool;
    includeIndirectReferences: bool;
  }
end

module GoToImplementation : sig
  type params = implementationParams

  and result = Location.t list

  and implementationParams = { loc: TextDocumentPositionParams.t }
end

module DocumentHighlight : sig
  type params = TextDocumentPositionParams.t

  type documentHighlightKind =
    | Text [@value 1]
    | Read [@value 2]
    | Write [@value 3]
  [@@deriving enum]

  type result = documentHighlight list

  and documentHighlight = {
    range: range;
    kind: documentHighlightKind option;
  }
end

module TypeCoverage : sig
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

module DocumentFormatting : sig
  type params = documentFormattingParams

  and result = TextEdit.t list

  and documentFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    options: formattingOptions;
  }

  and formattingOptions = {
    tabSize: int;
    insertSpaces: bool;
  }
end

module DocumentRangeFormatting : sig
  type params = documentRangeFormattingParams

  and result = TextEdit.t list

  and documentRangeFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    range: range;
    options: DocumentFormatting.formattingOptions;
  }
end

module DocumentOnTypeFormatting : sig
  type params = documentOnTypeFormattingParams

  and result = TextEdit.t list

  and documentOnTypeFormattingParams = {
    textDocument: TextDocumentIdentifier.t;
    position: position;
    ch: string;
    options: DocumentFormatting.formattingOptions;
  }
end

module SelectionRange : sig
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

module SignatureHelp : sig
  module TriggerKind : sig
    type t =
      | Invoked [@value 1]
      | TriggerCharacter [@value 2]
      | ContentChange [@value 3]
    [@@deriving enum]
  end

  module Documentation : sig
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
module PrepareRename : sig
  type params = TextDocumentPositionParams.t

  type result = range option
end

module Rename : sig
  type params = renameParams

  and result = WorkspaceEdit.t

  and renameParams = {
    textDocument: TextDocumentIdentifier.t;
    position: position;
    newName: string;
  }
end

module DocumentCodeLens : sig
  type params = codelensParams

  and result = CodeLens.t list

  and codelensParams = { textDocument: TextDocumentIdentifier.t }
end

module ExecuteCommand : sig
  type params = executeCommandParams

  and result = unit

  and executeCommandParams = {
    command: Command.name;
    arguments: Hh_json.json list option;
  }
end

module LogMessage : sig
  type params = logMessageParams

  and logMessageParams = {
    type_: MessageType.t;
    message: string;
  }
end

module ShowMessage : sig
  type params = showMessageParams

  and showMessageParams = {
    type_: MessageType.t;
    message: string;
  }
end

module ShowMessageRequest : sig
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

module ShowStatus : sig
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

module ConnectionStatus : sig
  type params = connectionStatusParams

  and connectionStatusParams = { isConnected: bool }
end

module ToggleTypeCoverage : sig
  type params = toggleTypeCoverageParams

  and toggleTypeCoverageParams = { toggle: bool }
end

module RenameFiles : sig
  type fileRename = {
    oldUri: DocumentUri.t;
    newUri: DocumentUri.t;
  }
end

module WillRenameFiles : sig
  type params = { files: RenameFiles.fileRename list }

  and result = WorkspaceEdit.t
end

module Error : sig
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

  exception LspException of t
end

module RegisterCapability : sig
  type params = { registrations: registration list }

  and registration = {
    id: string;
    method_: string;
    registerOptions: options;
  }

  and options =
    | DidChangeConfiguration  (** has no options *)
    | DidChangeWatchedFiles of DidChangeWatchedFiles.registerOptions

  val make_registration : options -> registration
end

module AutoCloseJsx : sig
  type params = TextDocumentPositionParams.t

  and result = string option
end

module DocumentPaste : sig
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

module LinkedEditingRange : sig
  type params = TextDocumentPositionParams.t

  and result = linkedEditingRanges option

  and linkedEditingRanges = {
    ranges: range list;
    wordPattern: string option;
  }
end

module RenameFileImports : sig
  type params = RenameFiles.fileRename

  and result = WorkspaceEdit.t
end

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

module IdKey : sig
  type t = lsp_id

  val compare : t -> t -> int
end

module IdSet : sig
  include module type of Flow_set.Make (IdKey)
end

module IdMap : sig
  include module type of WrappedMap.Make (IdKey)
end
