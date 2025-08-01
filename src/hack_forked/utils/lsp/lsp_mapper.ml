(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp

type t = {
  of_apply_workspace_edit_params: t -> ApplyWorkspaceEdit.params -> ApplyWorkspaceEdit.params;
  of_apply_workspace_edit_result: t -> ApplyWorkspaceEdit.result -> ApplyWorkspaceEdit.result;
  of_auto_close_jsx_params: t -> AutoCloseJsx.params -> AutoCloseJsx.params;
  of_auto_close_jsx_result: t -> AutoCloseJsx.result -> AutoCloseJsx.result;
  of_cancel_request_params: t -> CancelRequest.params -> CancelRequest.params;
  of_code_action: t -> CodeAction.t -> CodeAction.t;
  of_code_action_context:
    t -> CodeActionRequest.codeActionContext -> CodeActionRequest.codeActionContext;
  of_code_action_request_params: t -> CodeActionRequest.params -> CodeActionRequest.params;
  of_code_action_result: t -> CodeAction.result -> CodeAction.result;
  of_code_lens: t -> CodeLens.t -> CodeLens.t;
  of_code_lens_resolve_params: t -> CodeLensResolve.params -> CodeLensResolve.params;
  of_code_lens_resolve_result: t -> CodeLensResolve.result -> CodeLensResolve.result;
  of_command: t -> Command.t -> Command.t;
  of_completion_item: t -> Completion.completionItem -> Completion.completionItem;
  of_completion_params: t -> Completion.params -> Completion.params;
  of_completion_result: t -> Completion.result -> Completion.result;
  of_configuration_params: t -> Configuration.params -> Configuration.params;
  of_configuration_result: t -> Configuration.result -> Configuration.result;
  of_connection_status_params: t -> ConnectionStatus.params -> ConnectionStatus.params;
  of_definition_params: t -> Definition.params -> Definition.params;
  of_definition_result: t -> Definition.result -> Definition.result;
  of_diagnostic: t -> PublishDiagnostics.diagnostic -> PublishDiagnostics.diagnostic;
  of_did_change_configuration_params:
    t -> DidChangeConfiguration.params -> DidChangeConfiguration.params;
  of_did_change_content_change_event:
    t -> DidChange.textDocumentContentChangeEvent -> DidChange.textDocumentContentChangeEvent;
  of_did_change_params: t -> DidChange.params -> DidChange.params;
  of_did_change_watched_files_params:
    t -> DidChangeWatchedFiles.params -> DidChangeWatchedFiles.params;
  of_did_close_params: t -> DidClose.params -> DidClose.params;
  of_did_open_params: t -> DidOpen.params -> DidOpen.params;
  of_did_save_params: t -> DidSave.params -> DidSave.params;
  of_document_code_lens_params: t -> DocumentCodeLens.params -> DocumentCodeLens.params;
  of_document_code_lens_result: t -> DocumentCodeLens.result -> DocumentCodeLens.result;
  of_document_formatting_params: t -> DocumentFormatting.params -> DocumentFormatting.params;
  of_document_formatting_result: t -> DocumentFormatting.result -> DocumentFormatting.result;
  of_document_highlight_params: t -> DocumentHighlight.params -> DocumentHighlight.params;
  of_document_highlight_result: t -> DocumentHighlight.result -> DocumentHighlight.result;
  of_document_on_type_formatting_params:
    t -> DocumentOnTypeFormatting.params -> DocumentOnTypeFormatting.params;
  of_document_on_type_formatting_result:
    t -> DocumentOnTypeFormatting.result -> DocumentOnTypeFormatting.result;
  of_document_paste_prepare_params:
    t -> DocumentPaste.prepare_params -> DocumentPaste.prepare_params;
  of_document_paste_provide_params:
    t -> DocumentPaste.provide_params -> DocumentPaste.provide_params;
  of_document_range_formatting_params:
    t -> DocumentRangeFormatting.params -> DocumentRangeFormatting.params;
  of_document_range_formatting_result:
    t -> DocumentRangeFormatting.result -> DocumentRangeFormatting.result;
  of_document_symbol_params: t -> DocumentSymbol.params -> DocumentSymbol.params;
  of_document_symbol_result: t -> DocumentSymbol.result -> DocumentSymbol.result;
  of_document_symbol: t -> DocumentSymbol.t -> DocumentSymbol.t;
  of_document_uri: t -> DocumentUri.t -> DocumentUri.t;
  of_execute_command_params: t -> ExecuteCommand.params -> ExecuteCommand.params;
  of_execute_command_result: t -> ExecuteCommand.result -> ExecuteCommand.result;
  of_find_references_params: t -> FindReferences.params -> FindReferences.params;
  of_find_references_result: t -> FindReferences.result -> FindReferences.result;
  of_go_to_implementation_result: t -> GoToImplementation.result -> GoToImplementation.result;
  of_hover_params: t -> Hover.params -> Hover.params;
  of_hover_result: t -> Hover.result -> Hover.result;
  of_initialize_params: t -> Initialize.params -> Initialize.params;
  of_initialize_result: t -> Initialize.result -> Initialize.result;
  of_insert_replace_edit: t -> InsertReplaceEdit.t -> InsertReplaceEdit.t;
  of_linked_editing_range_params: t -> LinkedEditingRange.params -> LinkedEditingRange.params;
  of_linked_editing_range_result: t -> LinkedEditingRange.result -> LinkedEditingRange.result;
  of_log_message_params: t -> LogMessage.params -> LogMessage.params;
  of_lsp_message: t -> lsp_message -> lsp_message;
  of_lsp_notification: t -> lsp_notification -> lsp_notification;
  of_lsp_result: t -> lsp_result -> lsp_result;
  of_lsp_request: t -> lsp_request -> lsp_request;
  of_location: t -> Location.t -> Location.t;
  of_publish_diagnostics_params: t -> PublishDiagnostics.params -> PublishDiagnostics.params;
  of_rage_result: t -> Rage.result -> Rage.result;
  of_prepare_rename_params: t -> PrepareRename.params -> PrepareRename.params;
  of_prepare_rename_result: t -> PrepareRename.result -> PrepareRename.result;
  of_ping_result: t -> Ping.result -> Ping.result;
  of_range: t -> range -> range;
  of_register_capability_params: t -> RegisterCapability.params -> RegisterCapability.params;
  of_rename_params: t -> Rename.params -> Rename.params;
  of_rename_result: t -> Rename.result -> Rename.result;
  of_rename_file_imports_params: t -> RenameFileImports.params -> RenameFileImports.params;
  of_rename_file_imports_result: t -> RenameFileImports.result -> RenameFileImports.result;
  of_selection_range: t -> SelectionRange.selection_range -> SelectionRange.selection_range;
  of_selection_range_params: t -> SelectionRange.params -> SelectionRange.params;
  of_selection_range_result: t -> SelectionRange.result -> SelectionRange.result;
  of_signature_help_params: t -> SignatureHelp.params -> SignatureHelp.params;
  of_signature_help_result: t -> SignatureHelp.result -> SignatureHelp.result;
  of_text_document_diagnostics_params:
    t -> TextDocumentDiagnostics.params -> TextDocumentDiagnostics.params;
  of_text_document_diagnostics_result:
    t -> TextDocumentDiagnostics.result -> TextDocumentDiagnostics.result;
  of_show_message_params: t -> ShowMessage.params -> ShowMessage.params;
  of_show_message_request_params: t -> ShowMessageRequest.params -> ShowMessageRequest.params;
  of_show_message_request_result: t -> ShowMessageRequest.result -> ShowMessageRequest.result;
  of_show_status_params: t -> ShowStatus.params -> ShowStatus.params;
  of_show_status_result: t -> ShowStatus.result -> ShowStatus.result;
  of_symbol_information: t -> SymbolInformation.t -> SymbolInformation.t;
  of_workspace_symbol_information:
    t -> WorkspaceSymbolInformation.t -> WorkspaceSymbolInformation.t;
  of_text_document_identifier: t -> TextDocumentIdentifier.t -> TextDocumentIdentifier.t;
  of_text_document_item: t -> TextDocumentItem.t -> TextDocumentItem.t;
  of_text_document_position_params:
    t -> TextDocumentPositionParams.t -> TextDocumentPositionParams.t;
  of_text_edit: t -> TextEdit.t -> TextEdit.t;
  of_text_edit_or_insert_replace_edit:
    t ->
    [ `TextEdit of TextEdit.t | `InsertReplaceEdit of InsertReplaceEdit.t ] ->
    [ `TextEdit of TextEdit.t | `InsertReplaceEdit of InsertReplaceEdit.t ];
  of_type_coverage_params: t -> TypeCoverage.params -> TypeCoverage.params;
  of_type_coverage_result: t -> TypeCoverage.result -> TypeCoverage.result;
  of_type_definition_params: t -> TypeDefinition.params -> TypeDefinition.params;
  of_type_definition_result: t -> TypeDefinition.result -> TypeDefinition.result;
  of_versioned_text_document_identifier:
    t -> VersionedTextDocumentIdentifier.t -> VersionedTextDocumentIdentifier.t;
  of_will_rename_files_params: t -> WillRenameFiles.params -> WillRenameFiles.params;
  of_will_rename_files_result: t -> WillRenameFiles.result -> WillRenameFiles.result;
  of_workspace_edit: t -> WorkspaceEdit.t -> WorkspaceEdit.t;
  of_workspace_symbol_params: t -> WorkspaceSymbol.params -> WorkspaceSymbol.params;
  of_workspace_symbol_result: t -> WorkspaceSymbol.result -> WorkspaceSymbol.result;
}

let default_mapper =
  {
    of_apply_workspace_edit_params =
      (fun mapper { ApplyWorkspaceEdit.label; edit } ->
        { ApplyWorkspaceEdit.label; edit = mapper.of_workspace_edit mapper edit });
    of_apply_workspace_edit_result =
      (fun _mapper { ApplyWorkspaceEdit.applied; failureReason; failedChange } ->
        { ApplyWorkspaceEdit.applied; failureReason; failedChange });
    of_auto_close_jsx_params =
      (fun mapper params -> mapper.of_text_document_position_params mapper params);
    of_auto_close_jsx_result = (fun _mapper result -> result);
    of_cancel_request_params = (fun _mapper { CancelRequest.id } -> { CancelRequest.id });
    of_code_action =
      (fun mapper { CodeAction.title; kind; diagnostics; action } ->
        let diagnostics = Base.List.map ~f:(mapper.of_diagnostic mapper) diagnostics in
        let action =
          match action with
          | CodeAction.EditOnly edit -> CodeAction.EditOnly (mapper.of_workspace_edit mapper edit)
          | CodeAction.CommandOnly cmd -> CodeAction.CommandOnly (mapper.of_command mapper cmd)
          | CodeAction.BothEditThenCommand (edit, cmd) ->
            let edit = mapper.of_workspace_edit mapper edit in
            let cmd = mapper.of_command mapper cmd in
            CodeAction.BothEditThenCommand (edit, cmd)
        in
        { CodeAction.title; kind; diagnostics; action });
    of_code_action_context =
      (fun mapper { CodeActionRequest.diagnostics; only } ->
        let diagnostics = Base.List.map ~f:(mapper.of_diagnostic mapper) diagnostics in
        { CodeActionRequest.diagnostics; only });
    of_code_action_request_params =
      (fun mapper { CodeActionRequest.textDocument; range; context } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        let range = mapper.of_range mapper range in
        let context = mapper.of_code_action_context mapper context in
        { CodeActionRequest.textDocument; range; context });
    of_code_action_result =
      (fun mapper result ->
        Base.List.map
          ~f:(fun command_or_action ->
            match command_or_action with
            | CodeAction.Command t -> CodeAction.Command (mapper.of_command mapper t)
            | CodeAction.Action t -> CodeAction.Action (mapper.of_code_action mapper t))
          result);
    of_code_lens =
      (fun mapper { CodeLens.range; command; data } ->
        {
          CodeLens.range = mapper.of_range mapper range;
          command = mapper.of_command mapper command;
          data;
        });
    of_code_lens_resolve_params = (fun mapper t -> mapper.of_code_lens mapper t);
    of_code_lens_resolve_result = (fun mapper t -> mapper.of_code_lens mapper t);
    of_command =
      (fun _mapper { Command.title; command; arguments } -> { Command.title; command; arguments });
    of_completion_item =
      (fun mapper
           {
             Completion.label;
             labelDetails;
             kind;
             detail;
             documentation;
             tags;
             preselect;
             sortText;
             filterText;
             insertText;
             insertTextFormat;
             textEdit;
             additionalTextEdits;
             command;
             data;
           } ->
        let textEdit =
          Base.Option.map ~f:(mapper.of_text_edit_or_insert_replace_edit mapper) textEdit
        in
        let additionalTextEdits =
          Base.List.map ~f:(mapper.of_text_edit mapper) additionalTextEdits
        in
        let command = Base.Option.map ~f:(mapper.of_command mapper) command in
        {
          Completion.label;
          labelDetails;
          kind;
          detail;
          documentation;
          tags;
          preselect;
          sortText;
          filterText;
          insertText;
          insertTextFormat;
          textEdit;
          additionalTextEdits;
          command;
          data;
        });
    of_completion_params =
      (fun mapper { Completion.loc; context } ->
        let loc = mapper.of_text_document_position_params mapper loc in
        { Completion.loc; context });
    of_completion_result =
      (fun mapper { Completion.isIncomplete; items } ->
        let items = Base.List.map ~f:(mapper.of_completion_item mapper) items in
        { Completion.isIncomplete; items });
    of_configuration_params =
      (fun mapper { Configuration.items } ->
        let items =
          Base.List.map
            ~f:(fun { Configuration.scope_uri; section } ->
              let scope_uri = Base.Option.map ~f:(mapper.of_document_uri mapper) scope_uri in
              { Configuration.scope_uri; section })
            items
        in
        { Configuration.items });
    of_configuration_result = (fun _mapper json -> json);
    of_connection_status_params =
      (fun _mapper { ConnectionStatus.isConnected } -> { ConnectionStatus.isConnected });
    of_diagnostic =
      (fun mapper
           {
             PublishDiagnostics.range;
             severity;
             code;
             source;
             message;
             tags;
             relatedInformation;
             data;
           } ->
        let range = mapper.of_range mapper range in
        let map_related_info { PublishDiagnostics.relatedLocation; relatedMessage } =
          let relatedLocation = mapper.of_location mapper relatedLocation in
          { PublishDiagnostics.relatedLocation; relatedMessage }
        in
        let relatedInformation = Base.List.map ~f:map_related_info relatedInformation in
        {
          PublishDiagnostics.range;
          severity;
          code;
          source;
          message;
          tags;
          relatedInformation;
          data;
        });
    of_definition_params = (fun mapper t -> mapper.of_text_document_position_params mapper t);
    of_definition_result =
      (fun mapper results -> Base.List.map ~f:(mapper.of_location mapper) results);
    of_did_change_configuration_params =
      (fun _mapper { DidChangeConfiguration.settings } -> { DidChangeConfiguration.settings });
    of_did_change_content_change_event =
      (fun mapper { DidChange.range; rangeLength; text } ->
        { DidChange.range = Base.Option.map ~f:(mapper.of_range mapper) range; rangeLength; text });
    of_did_change_params =
      (fun mapper { DidChange.textDocument; contentChanges } ->
        let textDocument = mapper.of_versioned_text_document_identifier mapper textDocument in
        let contentChanges =
          Base.List.map ~f:(mapper.of_did_change_content_change_event mapper) contentChanges
        in
        { DidChange.textDocument; contentChanges });
    of_did_change_watched_files_params =
      (fun mapper { DidChangeWatchedFiles.changes } ->
        let changes =
          Base.List.map
            ~f:(fun { DidChangeWatchedFiles.uri; type_ } ->
              let uri = mapper.of_document_uri mapper uri in
              { DidChangeWatchedFiles.uri; type_ })
            changes
        in
        { DidChangeWatchedFiles.changes });
    of_did_close_params =
      (fun mapper { DidClose.textDocument } ->
        { DidClose.textDocument = mapper.of_text_document_identifier mapper textDocument });
    of_did_open_params =
      (fun mapper { DidOpen.textDocument } ->
        { DidOpen.textDocument = mapper.of_text_document_item mapper textDocument });
    of_did_save_params =
      (fun mapper { DidSave.textDocument; text } ->
        { DidSave.textDocument = mapper.of_text_document_identifier mapper textDocument; text });
    of_document_code_lens_params =
      (fun mapper { DocumentCodeLens.textDocument } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { DocumentCodeLens.textDocument });
    of_document_code_lens_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_code_lens mapper) result);
    of_document_formatting_params =
      (fun mapper { DocumentFormatting.textDocument; options } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { DocumentFormatting.textDocument; options });
    of_document_formatting_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_text_edit mapper) result);
    of_document_highlight_params =
      (fun mapper params -> mapper.of_text_document_position_params mapper params);
    of_document_highlight_result =
      (fun mapper result ->
        Base.List.map
          ~f:(fun { DocumentHighlight.range; kind } ->
            let range = mapper.of_range mapper range in
            { DocumentHighlight.range; kind })
          result);
    of_document_on_type_formatting_params =
      (fun mapper { DocumentOnTypeFormatting.textDocument; position; ch; options } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { DocumentOnTypeFormatting.textDocument; position; ch; options });
    of_document_on_type_formatting_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_text_edit mapper) result);
    of_document_paste_prepare_params =
      (fun mapper (DocumentPaste.PrepareParams { uri; ranges }) ->
        DocumentPaste.PrepareParams
          {
            uri = mapper.of_document_uri mapper uri;
            ranges = Base.List.map ranges ~f:(mapper.of_range mapper);
          });
    of_document_paste_provide_params =
      (fun mapper (DocumentPaste.ProvideParams { text_document; ranges; data_transfer }) ->
        DocumentPaste.ProvideParams
          {
            text_document = mapper.of_text_document_item mapper text_document;
            ranges = Base.List.map ranges ~f:(mapper.of_range mapper);
            data_transfer;
          });
    of_document_range_formatting_params =
      (fun mapper { DocumentRangeFormatting.textDocument; range; options } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        let range = mapper.of_range mapper range in
        { DocumentRangeFormatting.textDocument; range; options });
    of_document_range_formatting_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_text_edit mapper) result);
    of_document_symbol_params =
      (fun mapper { DocumentSymbol.textDocument } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { DocumentSymbol.textDocument });
    of_document_symbol_result =
      (fun mapper result ->
        match result with
        | `SymbolInformation result ->
          `SymbolInformation (Base.List.map ~f:(mapper.of_symbol_information mapper) result)
        | `DocumentSymbol result ->
          `DocumentSymbol (Base.List.map ~f:(mapper.of_document_symbol mapper) result));
    of_document_symbol =
      (fun mapper { DocumentSymbol.name; detail; kind; deprecated; range; selectionRange; children } ->
        let range = mapper.of_range mapper range in
        let selectionRange = mapper.of_range mapper selectionRange in
        let children =
          Base.Option.map ~f:(Base.List.map ~f:(mapper.of_document_symbol mapper)) children
        in
        { DocumentSymbol.name; detail; kind; deprecated; range; selectionRange; children });
    of_document_uri = (fun _mapper t -> t);
    of_execute_command_params =
      (fun _mapper { ExecuteCommand.command; arguments } -> { ExecuteCommand.command; arguments });
    of_execute_command_result = (fun _mapper () -> ());
    of_find_references_params =
      (fun mapper { FindReferences.loc; context } ->
        let loc = mapper.of_text_document_position_params mapper loc in
        { FindReferences.loc; context });
    of_find_references_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_location mapper) result);
    of_go_to_implementation_result =
      (fun mapper result -> Base.List.map ~f:(mapper.of_location mapper) result);
    of_hover_params = (fun mapper t -> mapper.of_text_document_position_params mapper t);
    of_hover_result =
      (fun mapper result ->
        match result with
        | Some { Hover.contents; range } ->
          Some
            {
              Hover.contents;
              range =
                (match range with
                | Some range -> Some (mapper.of_range mapper range)
                | None -> None);
            }
        | None -> None);
    of_initialize_params =
      (fun mapper
           {
             Initialize.processId;
             rootPath;
             rootUri;
             initializationOptions;
             client_capabilities;
             trace;
           } ->
        let rootUri = Base.Option.map ~f:(mapper.of_document_uri mapper) rootUri in
        (* TODO? Could add visitors for all of these options & capabilities *)
        {
          Initialize.processId;
          rootPath;
          rootUri;
          initializationOptions;
          client_capabilities;
          trace;
        });
    of_initialize_result =
      (fun _mapper { Initialize.server_capabilities; Initialize.server_info } ->
        (* TODO? Could add visitors for all of these capabilities *)
        { Initialize.server_capabilities; Initialize.server_info });
    of_insert_replace_edit =
      (fun mapper { InsertReplaceEdit.newText; insert; replace } ->
        let insert = mapper.of_range mapper insert in
        let replace = mapper.of_range mapper replace in
        { InsertReplaceEdit.newText; insert; replace });
    of_linked_editing_range_params =
      (fun mapper params -> mapper.of_text_document_position_params mapper params);
    of_linked_editing_range_result =
      (fun mapper result ->
        Base.Option.map result ~f:(fun { LinkedEditingRange.ranges; wordPattern } ->
            {
              LinkedEditingRange.ranges = Base.List.map ranges ~f:(mapper.of_range mapper);
              wordPattern;
            }
        ));
    of_log_message_params =
      (fun _mapper { LogMessage.type_; message } -> { LogMessage.type_; message });
    of_lsp_message =
      (fun mapper message ->
        match message with
        | RequestMessage (lsp_id, lsp_request) ->
          RequestMessage (lsp_id, mapper.of_lsp_request mapper lsp_request)
        | ResponseMessage (lsp_id, lsp_result) ->
          ResponseMessage (lsp_id, mapper.of_lsp_result mapper lsp_result)
        | NotificationMessage lsp_notification ->
          NotificationMessage (mapper.of_lsp_notification mapper lsp_notification));
    of_lsp_notification =
      (fun mapper t ->
        match t with
        | ExitNotification -> ExitNotification
        | CancelRequestNotification params ->
          CancelRequestNotification (mapper.of_cancel_request_params mapper params)
        | PublishDiagnosticsNotification params ->
          PublishDiagnosticsNotification (mapper.of_publish_diagnostics_params mapper params)
        | DidOpenNotification params -> DidOpenNotification (mapper.of_did_open_params mapper params)
        | DidCloseNotification params ->
          DidCloseNotification (mapper.of_did_close_params mapper params)
        | DidSaveNotification params -> DidSaveNotification (mapper.of_did_save_params mapper params)
        | DidChangeNotification params ->
          DidChangeNotification (mapper.of_did_change_params mapper params)
        | DidChangeConfigurationNotification params ->
          DidChangeConfigurationNotification
            (mapper.of_did_change_configuration_params mapper params)
        | DidChangeWatchedFilesNotification params ->
          DidChangeWatchedFilesNotification (mapper.of_did_change_watched_files_params mapper params)
        | LogMessageNotification params ->
          LogMessageNotification (mapper.of_log_message_params mapper params)
        | TelemetryNotification params ->
          TelemetryNotification (mapper.of_log_message_params mapper params)
        | ShowMessageNotification params ->
          ShowMessageNotification (mapper.of_show_message_params mapper params)
        | ConnectionStatusNotification params ->
          ConnectionStatusNotification (mapper.of_connection_status_params mapper params)
        | InitializedNotification -> InitializedNotification
        | SetTraceNotification -> SetTraceNotification
        | LogTraceNotification -> LogTraceNotification
        | UnknownNotification (msg, json) -> UnknownNotification (msg, json));
    of_lsp_result =
      (fun mapper result ->
        match result with
        | InitializeResult result -> InitializeResult (mapper.of_initialize_result mapper result)
        | ShutdownResult -> ShutdownResult
        | CodeLensResolveResult result ->
          CodeLensResolveResult (mapper.of_code_lens_resolve_result mapper result)
        | HoverResult result -> HoverResult (mapper.of_hover_result mapper result)
        | DefinitionResult result -> DefinitionResult (mapper.of_definition_result mapper result)
        | TypeDefinitionResult result ->
          TypeDefinitionResult (mapper.of_type_definition_result mapper result)
        | CodeActionResult result -> CodeActionResult (mapper.of_code_action_result mapper result)
        | CompletionResult result -> CompletionResult (mapper.of_completion_result mapper result)
        | CompletionItemResolveResult result ->
          CompletionItemResolveResult (mapper.of_completion_item mapper result)
        | ConfigurationResult result ->
          ConfigurationResult (mapper.of_configuration_result mapper result)
        | SelectionRangeResult result ->
          SelectionRangeResult (mapper.of_selection_range_result mapper result)
        | SignatureHelpResult result ->
          SignatureHelpResult (mapper.of_signature_help_result mapper result)
        | TextDocumentDiagnosticsResult result ->
          TextDocumentDiagnosticsResult (mapper.of_text_document_diagnostics_result mapper result)
        | WorkspaceSymbolResult result ->
          WorkspaceSymbolResult (mapper.of_workspace_symbol_result mapper result)
        | DocumentSymbolResult result ->
          DocumentSymbolResult (mapper.of_document_symbol_result mapper result)
        | FindReferencesResult result ->
          FindReferencesResult (mapper.of_find_references_result mapper result)
        | GoToImplementationResult result ->
          GoToImplementationResult (mapper.of_go_to_implementation_result mapper result)
        | DocumentHighlightResult result ->
          DocumentHighlightResult (mapper.of_document_highlight_result mapper result)
        | TypeCoverageResult result ->
          TypeCoverageResult (mapper.of_type_coverage_result mapper result)
        | DocumentFormattingResult result ->
          DocumentFormattingResult (mapper.of_document_formatting_result mapper result)
        | DocumentRangeFormattingResult result ->
          DocumentRangeFormattingResult (mapper.of_document_range_formatting_result mapper result)
        | DocumentOnTypeFormattingResult result ->
          DocumentOnTypeFormattingResult (mapper.of_document_on_type_formatting_result mapper result)
        | ShowMessageRequestResult result ->
          ShowMessageRequestResult (mapper.of_show_message_request_result mapper result)
        | ShowStatusResult result -> ShowStatusResult (mapper.of_show_status_result mapper result)
        | RageResult result -> RageResult (mapper.of_rage_result mapper result)
        | PrepareRenameResult result ->
          PrepareRenameResult (mapper.of_prepare_rename_result mapper result)
        | PingResult result -> PingResult (mapper.of_ping_result mapper result)
        | RegisterCapabilityResult -> RegisterCapabilityResult
        | RenameResult result -> RenameResult (mapper.of_rename_result mapper result)
        | DocumentCodeLensResult result ->
          DocumentCodeLensResult (mapper.of_document_code_lens_result mapper result)
        | ExecuteCommandResult result ->
          ExecuteCommandResult (mapper.of_execute_command_result mapper result)
        | ApplyWorkspaceEditResult result ->
          ApplyWorkspaceEditResult (mapper.of_apply_workspace_edit_result mapper result)
        | WillRenameFilesResult result ->
          WillRenameFilesResult (mapper.of_will_rename_files_result mapper result)
        | AutoCloseJsxResult result ->
          AutoCloseJsxResult (mapper.of_auto_close_jsx_result mapper result)
        | PrepareDocumentPasteResult r -> PrepareDocumentPasteResult r
        | ProvideDocumentPasteResult r ->
          ProvideDocumentPasteResult (mapper.of_workspace_edit mapper r)
        | LinkedEditingRangeResult result ->
          LinkedEditingRangeResult (mapper.of_linked_editing_range_result mapper result)
        | RenameFileImportsResult result ->
          RenameFileImportsResult (mapper.of_rename_file_imports_result mapper result)
        | ErrorResult (err, str) -> ErrorResult (err, str));
    of_lsp_request =
      (fun mapper request ->
        match request with
        | InitializeRequest params -> InitializeRequest (mapper.of_initialize_params mapper params)
        | RegisterCapabilityRequest params ->
          RegisterCapabilityRequest (mapper.of_register_capability_params mapper params)
        | ShutdownRequest -> ShutdownRequest
        | CodeLensResolveRequest params ->
          CodeLensResolveRequest (mapper.of_code_lens_resolve_params mapper params)
        | HoverRequest params -> HoverRequest (mapper.of_hover_params mapper params)
        | DefinitionRequest params -> DefinitionRequest (mapper.of_definition_params mapper params)
        | TypeDefinitionRequest params ->
          TypeDefinitionRequest (mapper.of_type_definition_params mapper params)
        | CodeActionRequest params ->
          CodeActionRequest (mapper.of_code_action_request_params mapper params)
        | CompletionRequest params -> CompletionRequest (mapper.of_completion_params mapper params)
        | CompletionItemResolveRequest params ->
          CompletionItemResolveRequest (mapper.of_completion_item mapper params)
        | ConfigurationRequest params ->
          ConfigurationRequest (mapper.of_configuration_params mapper params)
        | SelectionRangeRequest params ->
          SelectionRangeRequest (mapper.of_selection_range_params mapper params)
        | SignatureHelpRequest params ->
          SignatureHelpRequest (mapper.of_signature_help_params mapper params)
        | TextDocumentDiagnosticsRequest params ->
          TextDocumentDiagnosticsRequest (mapper.of_text_document_diagnostics_params mapper params)
        | WorkspaceSymbolRequest params ->
          WorkspaceSymbolRequest (mapper.of_workspace_symbol_params mapper params)
        | DocumentSymbolRequest params ->
          DocumentSymbolRequest (mapper.of_document_symbol_params mapper params)
        | FindReferencesRequest params ->
          FindReferencesRequest (mapper.of_find_references_params mapper params)
        | DocumentHighlightRequest params ->
          DocumentHighlightRequest (mapper.of_document_highlight_params mapper params)
        | TypeCoverageRequest params ->
          TypeCoverageRequest (mapper.of_type_coverage_params mapper params)
        | DocumentFormattingRequest params ->
          DocumentFormattingRequest (mapper.of_document_formatting_params mapper params)
        | DocumentRangeFormattingRequest params ->
          DocumentRangeFormattingRequest (mapper.of_document_range_formatting_params mapper params)
        | DocumentOnTypeFormattingRequest params ->
          DocumentOnTypeFormattingRequest
            (mapper.of_document_on_type_formatting_params mapper params)
        | ShowMessageRequestRequest params ->
          ShowMessageRequestRequest (mapper.of_show_message_request_params mapper params)
        | ShowStatusRequest params -> ShowStatusRequest (mapper.of_show_status_params mapper params)
        | RageRequest -> RageRequest
        | PingRequest -> PingRequest
        | PrepareRenameRequest params ->
          PrepareRenameRequest (mapper.of_prepare_rename_params mapper params)
        | RenameRequest params -> RenameRequest (mapper.of_rename_params mapper params)
        | DocumentCodeLensRequest params ->
          DocumentCodeLensRequest (mapper.of_document_code_lens_params mapper params)
        | ExecuteCommandRequest params ->
          ExecuteCommandRequest (mapper.of_execute_command_params mapper params)
        | ApplyWorkspaceEditRequest params ->
          ApplyWorkspaceEditRequest (mapper.of_apply_workspace_edit_params mapper params)
        | WillRenameFilesRequest params ->
          WillRenameFilesRequest (mapper.of_will_rename_files_params mapper params)
        | AutoCloseJsxRequest params ->
          AutoCloseJsxRequest (mapper.of_auto_close_jsx_params mapper params)
        | PrepareDocumentPasteRequest params ->
          PrepareDocumentPasteRequest (mapper.of_document_paste_prepare_params mapper params)
        | ProvideDocumentPasteRequest params ->
          ProvideDocumentPasteRequest (mapper.of_document_paste_provide_params mapper params)
        | LinkedEditingRangeRequest params ->
          LinkedEditingRangeRequest (mapper.of_linked_editing_range_params mapper params)
        | RenameFileImportsRequest params ->
          RenameFileImportsRequest (mapper.of_rename_file_imports_params mapper params)
        | UnknownRequest (req, json) -> UnknownRequest (req, json));
    of_location =
      (fun mapper { Location.uri; range } ->
        { Location.uri = mapper.of_document_uri mapper uri; range });
    of_publish_diagnostics_params =
      (fun mapper { PublishDiagnostics.uri; diagnostics } ->
        let uri = mapper.of_document_uri mapper uri in
        let diagnostics = Base.List.map ~f:(mapper.of_diagnostic mapper) diagnostics in
        { PublishDiagnostics.uri; diagnostics });
    of_rage_result = (fun _mapper t -> t);
    of_prepare_rename_params =
      (fun mapper pos -> mapper.of_text_document_position_params mapper pos);
    of_prepare_rename_result = (fun mapper result -> Option.map (mapper.of_range mapper) result);
    of_ping_result = (fun _mapper t -> t);
    of_range = (fun _mapper t -> t);
    of_register_capability_params =
      (fun _mapper { RegisterCapability.registrations } ->
        let open RegisterCapability in
        let registrations =
          Base.List.map
            ~f:(fun { id; method_; registerOptions } ->
              let registerOptions =
                match registerOptions with
                | DidChangeConfiguration -> DidChangeConfiguration
                | DidChangeWatchedFiles opts -> DidChangeWatchedFiles opts
              in
              { id; method_; registerOptions })
            registrations
        in
        { registrations });
    of_rename_params =
      (fun mapper { Rename.textDocument; position; newName } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { Rename.textDocument; position; newName });
    of_rename_result = (fun mapper result -> mapper.of_workspace_edit mapper result);
    of_rename_file_imports_params =
      (fun mapper { RenameFiles.oldUri; newUri } ->
        {
          RenameFiles.oldUri = mapper.of_document_uri mapper oldUri;
          newUri = mapper.of_document_uri mapper newUri;
        });
    of_rename_file_imports_result = (fun mapper result -> mapper.of_workspace_edit mapper result);
    of_selection_range_params =
      (fun mapper { SelectionRange.textDocument; positions } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { SelectionRange.textDocument; positions });
    of_selection_range =
      (fun mapper { SelectionRange.range; parent } ->
        let range = mapper.of_range mapper range in
        let parent = Base.Option.map ~f:(mapper.of_selection_range mapper) parent in
        { SelectionRange.range; parent });
    of_selection_range_result = (fun mapper -> Base.List.map ~f:(mapper.of_selection_range mapper));
    of_signature_help_params =
      (fun mapper { SignatureHelp.loc; context } ->
        let loc = mapper.of_text_document_position_params mapper loc in
        { SignatureHelp.loc; context });
    of_signature_help_result = (fun _mapper t -> t);
    of_text_document_diagnostics_params =
      (fun mapper { TextDocumentDiagnostics.textDocument } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { TextDocumentDiagnostics.textDocument });
    of_text_document_diagnostics_result =
      (fun mapper list -> Base.List.map ~f:(mapper.of_diagnostic mapper) list);
    of_show_message_params =
      (fun _mapper { ShowMessage.type_; message } -> { ShowMessage.type_; message });
    of_show_message_request_params =
      (fun _mapper { ShowMessageRequest.type_; message; actions } ->
        { ShowMessageRequest.type_; message; actions });
    of_show_message_request_result = (fun _mapper t -> t);
    of_show_status_params = (fun _mapper t -> t);
    of_show_status_result = (fun _mapper t -> t);
    of_symbol_information =
      (fun mapper { SymbolInformation.name; kind; location; selectionRange; containerName } ->
        let location = mapper.of_location mapper location in
        { SymbolInformation.name; kind; location; selectionRange; containerName });
    of_workspace_symbol_information =
      (fun mapper { WorkspaceSymbolInformation.name; kind; location; containerName } ->
        let location = mapper.of_text_document_identifier mapper location in
        { WorkspaceSymbolInformation.name; kind; location; containerName });
    of_text_document_identifier =
      (fun mapper { TextDocumentIdentifier.uri } ->
        { TextDocumentIdentifier.uri = mapper.of_document_uri mapper uri });
    of_text_document_item =
      (fun mapper { TextDocumentItem.uri; languageId; version; text } ->
        { TextDocumentItem.uri = mapper.of_document_uri mapper uri; languageId; version; text });
    of_text_document_position_params =
      (fun mapper { TextDocumentPositionParams.textDocument; position } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { TextDocumentPositionParams.textDocument; position });
    of_text_edit =
      (fun mapper { TextEdit.range; newText } ->
        let range = mapper.of_range mapper range in
        { TextEdit.range; newText });
    of_text_edit_or_insert_replace_edit =
      (fun mapper edit ->
        match edit with
        | `TextEdit edit -> `TextEdit (mapper.of_text_edit mapper edit)
        | `InsertReplaceEdit edit -> `InsertReplaceEdit (mapper.of_insert_replace_edit mapper edit));
    of_type_coverage_params =
      (fun mapper { TypeCoverage.textDocument } ->
        let textDocument = mapper.of_text_document_identifier mapper textDocument in
        { TypeCoverage.textDocument });
    of_type_coverage_result =
      (fun mapper { TypeCoverage.coveredPercent; uncoveredRanges; defaultMessage } ->
        let uncoveredRanges =
          Base.List.map
            ~f:(fun { TypeCoverage.range; message } ->
              let range = mapper.of_range mapper range in
              { TypeCoverage.range; message })
            uncoveredRanges
        in
        { TypeCoverage.coveredPercent; uncoveredRanges; defaultMessage });
    of_type_definition_params =
      (fun mapper params -> mapper.of_text_document_position_params mapper params);
    of_type_definition_result =
      (fun mapper results -> Base.List.map ~f:(mapper.of_location mapper) results);
    of_versioned_text_document_identifier =
      (fun mapper { VersionedTextDocumentIdentifier.uri; version } ->
        let uri = mapper.of_document_uri mapper uri in
        { VersionedTextDocumentIdentifier.uri; version });
    of_will_rename_files_params =
      (fun mapper { WillRenameFiles.files } ->
        let files =
          Base.List.map
            ~f:(fun { RenameFiles.oldUri; newUri } ->
              {
                RenameFiles.oldUri = mapper.of_document_uri mapper oldUri;
                newUri = mapper.of_document_uri mapper newUri;
              })
            files
        in
        { WillRenameFiles.files });
    of_will_rename_files_result = (fun mapper result -> mapper.of_workspace_edit mapper result);
    of_workspace_edit =
      (fun mapper { WorkspaceEdit.changes } ->
        let changes =
          UriMap.fold
            (fun uri edits acc ->
              let uri = mapper.of_document_uri mapper uri in
              let edits = Base.List.map ~f:(mapper.of_text_edit mapper) edits in
              UriMap.add uri edits acc)
            changes
            UriMap.empty
        in
        { WorkspaceEdit.changes });
    of_workspace_symbol_params =
      (fun _mapper { WorkspaceSymbol.query } -> { WorkspaceSymbol.query });
    of_workspace_symbol_result =
      (fun mapper -> function
        | WorkspaceSymbol.SymbolInformation info ->
          WorkspaceSymbol.SymbolInformation
            (Base.List.map info ~f:(mapper.of_symbol_information mapper))
        | WorkspaceSymbol.WorkspaceSymbolInformation info ->
          WorkspaceSymbol.WorkspaceSymbolInformation
            (Base.List.map info ~f:(mapper.of_workspace_symbol_information mapper)));
  }
