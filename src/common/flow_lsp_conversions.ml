(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let markup_string str = { Lsp.MarkupContent.kind = Lsp.MarkupKind.Markdown; value = str }

let selection_range_of_loc ?parent (loc : Loc.t) : Lsp.SelectionRange.selection_range =
  { Lsp.SelectionRange.range = Lsp.loc_to_lsp_range loc; parent }

let func_details_result_to_lsp =
  let open Lsp.SignatureHelp in
  function
  | ServerProt.Response.SigHelpFunc { param_tys; return_ty; func_documentation } ->
    let doc_opt = Base.Option.map ~f:(fun doc -> Documentation.MarkupContent (markup_string doc)) in
    let label_buf = Buffer.create 20 in
    Buffer.add_string label_buf "(";
    let parameters =
      param_tys
      |> Base.List.mapi
           ~f:(fun i { ServerProt.Response.param_name; param_ty; param_documentation } ->
             let label = Printf.sprintf "%s: %s" param_name param_ty in
             if i > 0 then Buffer.add_string label_buf ", ";
             Buffer.add_string label_buf label;
             { parinfo_label = String label; parinfo_documentation = doc_opt param_documentation }
         )
    in
    Buffer.add_string label_buf "): ";
    Buffer.add_string label_buf return_ty;
    let siginfo_label = Buffer.contents label_buf in
    let siginfo_documentation = doc_opt func_documentation in
    { siginfo_label; siginfo_documentation; parameters }
  | ServerProt.Response.SigHelpJsxAttr { documentation; name; ty; optional } ->
    let doc_opt =
      Base.Option.map ~f:(fun doc -> Documentation.MarkupContent (markup_string doc)) documentation
    in
    let label_buf = Buffer.create 20 in
    let parameters =
      let label = Printf.sprintf "%s%s: %s" name (Utils_js.ite optional "?" "") ty in
      Buffer.add_string label_buf label;
      [{ parinfo_label = String label; parinfo_documentation = doc_opt }]
    in
    let siginfo_label = Buffer.contents label_buf in
    let siginfo_documentation = None in
    { siginfo_label; siginfo_documentation; parameters }

let flow_signature_help_to_lsp
    (details : (ServerProt.Response.func_details_result list * int) option) :
    Lsp.SignatureHelp.result =
  match details with
  | None -> None
  | Some (signatures, active_parameter) ->
    let open Lsp.SignatureHelp in
    let signatures = Base.List.map signatures ~f:func_details_result_to_lsp in
    Some { signatures; activeSignature = 0; activeParameter = active_parameter }

let flow_completion_item_to_lsp
    ?token
    ?autocomplete_session_length
    ?typed_len
    ~ac_type
    ~is_snippet_supported:(_ : bool)
    ~(is_tags_supported : Lsp.CompletionItemTag.t -> bool)
    ~(is_preselect_supported : bool)
    ~(is_label_detail_supported : bool)
    ~(is_insert_replace_supported : bool)
    ~index
    (item : ServerProt.Response.Completion.completion_item) : Lsp.Completion.completionItem =
  let open ServerProt.Response.Completion in
  let textEdit =
    Base.Option.map
      ~f:(fun { ServerProt.Response.newText; insert; replace } ->
        if is_insert_replace_supported && not (Loc.equal insert replace) then
          `InsertReplaceEdit
            {
              Lsp.InsertReplaceEdit.newText;
              insert = Lsp.loc_to_lsp_range insert;
              replace = Lsp.loc_to_lsp_range replace;
            }
        else
          `TextEdit { Lsp.TextEdit.range = Lsp.loc_to_lsp_range insert; newText })
      item.text_edit
  in
  let additionalTextEdits =
    Base.List.map
      ~f:(fun (loc, newText) -> { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText })
      item.additional_text_edits
  in
  let documentation = Base.Option.map item.documentation ~f:(fun doc -> [Lsp.MarkedString doc]) in
  let tags = Base.Option.map item.tags ~f:(List.filter is_tags_supported) in
  let command =
    let open Lsp.Command in
    Some
      {
        title = "";
        command = Command "log";
        arguments =
          Hh_json.
            [
              JSON_String "textDocument/completion";
              JSON_String item.log_info;
              JSON_Object
                [
                  ( "token",
                    match token with
                    | None -> JSON_Null
                    | Some token -> JSON_String token
                  );
                  ("index", JSON_Number (string_of_int index));
                  ( "session_requests",
                    match autocomplete_session_length with
                    | None -> JSON_Null
                    | Some autocomplete_session_length ->
                      JSON_Number (string_of_int autocomplete_session_length)
                  );
                  ( "typed_length",
                    match typed_len with
                    | None -> JSON_Null
                    | Some typed_length -> JSON_Number (string_of_int typed_length)
                  );
                  ("completion", JSON_String item.name);
                  ("ac_type", JSON_String ac_type);
                ];
            ];
      }
  in
  let labelDetails =
    if
      is_label_detail_supported
      && (Base.Option.is_some item.labelDetail || Base.Option.is_some item.description)
    then
      let detail =
        let trunc n s =
          if String.length s < n then
            s
          else
            String.sub s 0 n ^ "..."
        in
        let column_width = 80 in
        Base.Option.map ~f:(trunc column_width) item.labelDetail
      in
      Some { Lsp.CompletionItemLabelDetails.detail; description = item.description }
    else
      None
  in
  {
    Lsp.Completion.label = item.name;
    labelDetails;
    kind = item.kind;
    detail = item.itemDetail;
    documentation;
    tags;
    preselect = is_preselect_supported && item.preselect;
    sortText = item.sort_text;
    filterText = None;
    insertText = None (* deprecated and should not be used *);
    insertTextFormat = Some item.insert_text_format;
    textEdit;
    additionalTextEdits;
    command;
    data = None;
  }

let flow_completions_to_lsp
    ?token
    ?autocomplete_session_length
    ?typed_len
    ~ac_type
    ~(is_snippet_supported : bool)
    ~(is_tags_supported : Lsp.CompletionItemTag.t -> bool)
    ~(is_preselect_supported : bool)
    ~(is_label_detail_supported : bool)
    ~(is_insert_replace_supported : bool)
    (completions : ServerProt.Response.Completion.t) : Lsp.Completion.result =
  let { ServerProt.Response.Completion.items; is_incomplete } = completions in
  let open ServerProt.Response.Completion in
  let items =
    Base.List.mapi
      ~f:(fun index item ->
        flow_completion_item_to_lsp
          ?token
          ?autocomplete_session_length
          ?typed_len
          ~ac_type
          ~is_snippet_supported
          ~is_tags_supported
          ~is_preselect_supported
          ~is_label_detail_supported
          ~is_insert_replace_supported
          ~index
          { item with sort_text = Option.some (Printf.sprintf "%020u" index) })
      items
  in
  { Lsp.Completion.isIncomplete = is_incomplete; items }

let file_key_to_uri (file_key_opt : File_key.t option) : (Lsp.DocumentUri.t, string) result =
  let ( >>| ) = Base.Result.( >>| ) in
  let ( >>= ) = Base.Result.( >>= ) in
  Base.Result.of_option file_key_opt ~error:"File_key is None"
  >>= File_key.to_path
  >>| File_url.create
  >>| Lsp.DocumentUri.of_string

let loc_to_lsp (loc : Loc.t) : (Lsp.Location.t, string) result =
  let ( >>| ) = Base.Result.( >>| ) in
  file_key_to_uri loc.Loc.source >>| fun uri ->
  { Lsp.Location.uri; range = Lsp.loc_to_lsp_range loc }

let loc_to_lsp_with_default (loc : Loc.t) ~(default_uri : Lsp.DocumentUri.t) : Lsp.Location.t =
  let uri =
    match file_key_to_uri loc.Loc.source with
    | Ok uri -> uri
    | Error _ -> default_uri
  in
  { Lsp.Location.uri; range = Lsp.loc_to_lsp_range loc }

let flow_edit_to_textedit (edit : Loc.t * string) : Lsp.TextEdit.t =
  let (loc, text) = edit in
  { Lsp.TextEdit.range = Lsp.loc_to_lsp_range loc; newText = text }

(* ~, . and .. have no meaning in file urls so we don't canonicalize them *)
(* but symlinks must be canonicalized before being used in flow: *)
let lsp_DocumentIdentifier_to_flow_path textDocument =
  let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
  Sys_utils.realpath fn |> Base.Option.value ~default:fn

let position_of_document_position { Lsp.TextDocumentPositionParams.position; _ } =
  Lsp.lsp_position_to_flow position

let diagnostics_of_flow_errors =
  let error_to_lsp
      ~unsaved_content
      ~should_include_vscode_detailed_diagnostics
      ~(severity : Severity.severity)
      (printable_error : Loc.t Flow_errors_utils.printable_error) :
      (Lsp.DocumentUri.t * Lsp.PublishDiagnostics.diagnostic) option =
    let has_detailed_diagnostics = should_include_vscode_detailed_diagnostics printable_error in
    let lsp_error =
      Flow_errors_utils.Lsp_output.lsp_of_error ~has_detailed_diagnostics printable_error
    in
    match loc_to_lsp lsp_error.Flow_errors_utils.Lsp_output.loc with
    | Ok location ->
      let uri = location.Lsp.Location.uri in
      let related_to_lsp (loc, relatedMessage) =
        match loc_to_lsp loc with
        | Ok relatedLocation -> Some { Lsp.PublishDiagnostics.relatedLocation; relatedMessage }
        | Error _ -> None
      in
      let relatedInformation =
        Base.List.filter_map
          lsp_error.Flow_errors_utils.Lsp_output.relatedLocations
          ~f:related_to_lsp
      in
      let data =
        if has_detailed_diagnostics then
          let map_color = function
            | Tty.Default -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Default
            | Tty.Black -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Black
            | Tty.Red -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Red
            | Tty.Green -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Green
            | Tty.Yellow -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Yellow
            | Tty.Blue -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Blue
            | Tty.Magenta -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Magenta
            | Tty.Cyan -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Cyan
            | Tty.White -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.White
          in
          let map_style = function
            | Tty.Normal c -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Normal (map_color c)
            | Tty.Bold c -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Bold (map_color c)
            | Tty.Dim c -> Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Dim (map_color c)
            | Tty.Underline c ->
              Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.Underline (map_color c)
            | Tty.BoldUnderline c ->
              Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.BoldUnderline (map_color c)
            | Tty.DimUnderline c ->
              Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0.DimUnderline (map_color c)
          in
          Lsp.PublishDiagnostics.ExtraDetailedDiagnosticV0
            (Flow_errors_utils.Cli_output.format_single_styled_error_for_vscode
               ~strip_root:None
               ~severity
               ~unsaved_content
               printable_error
            |> Base.List.map ~f:(fun (style, text) -> (map_style style, text))
            )
        else
          Lsp.PublishDiagnostics.NoExtraData
      in
      Some
        ( uri,
          {
            Lsp.PublishDiagnostics.range = location.Lsp.Location.range;
            severity =
              (match severity with
              | Severity.Err -> Some Lsp.PublishDiagnostics.Error
              | Severity.Warn -> Some Lsp.PublishDiagnostics.Warning
              | Severity.Off -> None);
            code = Lsp.PublishDiagnostics.StringCode lsp_error.Flow_errors_utils.Lsp_output.code;
            source = Some "Flow";
            message = lsp_error.Flow_errors_utils.Lsp_output.message;
            tags = [];
            relatedInformation;
            data;
          }
        )
    | Error _ -> None
  in
  fun ~unsaved_content ~should_include_vscode_detailed_diagnostics ~errors ~warnings ->
    let add severity error acc =
      match
        error_to_lsp ~unsaved_content ~should_include_vscode_detailed_diagnostics ~severity error
      with
      | Some (uri, diagnostic) -> Lsp.UriMap.add ~combine:List.append uri [diagnostic] acc
      | None -> acc
    in
    Lsp.UriMap.empty
    |> Flow_errors_utils.ConcreteLocPrintableErrorSet.fold (add Severity.Err) errors
    |> Flow_errors_utils.ConcreteLocPrintableErrorSet.fold (add Severity.Warn) warnings

let synthetic_diagnostics_of_switch_to_match_eligible_locations locs =
  Base.List.filter_map locs ~f:(fun loc -> loc |> loc_to_lsp |> Result.to_option)
  |> Base.List.map ~f:(fun location ->
         {
           Lsp.PublishDiagnostics.range = location.Lsp.Location.range;
           severity = Some Lsp.PublishDiagnostics.Hint;
           code = Lsp.PublishDiagnostics.StringCode "switch-to-match";
           source = Some "Flow";
           message = "This switch statement can be converted to use match syntax";
           tags = [];
           relatedInformation = [];
           data = Lsp.PublishDiagnostics.NoExtraData;
         }
     )

let synthetic_diagnostics_of_refined_locations locs =
  Base.List.filter_map locs ~f:(fun loc -> loc |> loc_to_lsp |> Result.to_option)
  |> Base.List.map ~f:(fun location ->
         {
           Lsp.PublishDiagnostics.range = location.Lsp.Location.range;
           severity = Some Lsp.PublishDiagnostics.Hint;
           code = Lsp.PublishDiagnostics.NoCode;
           source = Some "Flow";
           message = "refined-value";
           tags = [];
           relatedInformation = [];
           data = Lsp.PublishDiagnostics.RefinementInformation;
         }
     )
