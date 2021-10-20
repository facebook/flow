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
    (* Flow's line numbers are 1-indexed; LSP's are 0-indexed *)
    let line = position.line + 1 in
    let char = position.character in
    (line, char)
  )

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
    let end_ = flow_position_to_lsp loc_end.line loc_end.column in
    { Lsp.start; end_ }
  )

let markup_string str = { Lsp.MarkupContent.kind = Lsp.MarkupKind.Markdown; value = str }

let selection_range_of_loc ?parent (loc : Loc.t) : Lsp.SelectionRange.selection_range =
  { Lsp.SelectionRange.range = loc_to_lsp_range loc; parent }

let flow_signature_help_to_lsp
    (details : (ServerProt.Response.func_details_result list * int) option) :
    Lsp.SignatureHelp.result =
  match details with
  | None -> None
  | Some (signatures, active_parameter) ->
    let open Lsp.SignatureHelp in
    let signatures =
      Base.List.fold_left
        signatures
        ~f:(fun acc { ServerProt.Response.param_tys; return_ty; func_documentation } ->
          let doc_opt =
            Base.Option.map ~f:(fun doc -> Documentation.MarkupContent (markup_string doc))
          in
          let label_buf = Buffer.create 20 in
          Buffer.add_string label_buf "(";
          let parameters =
            param_tys
            |> Base.List.mapi
                 ~f:(fun i { ServerProt.Response.param_name; param_ty; param_documentation } ->
                   let label = Printf.sprintf "%s: %s" param_name param_ty in
                   if i > 0 then Buffer.add_string label_buf ", ";
                   Buffer.add_string label_buf label;
                   {
                     parinfo_label = String label;
                     parinfo_documentation = doc_opt param_documentation;
                   }
               )
          in
          Buffer.add_string label_buf "): ";
          Buffer.add_string label_buf return_ty;
          let siginfo_label = Buffer.contents label_buf in
          let siginfo_documentation = doc_opt func_documentation in
          let signature = { siginfo_label; siginfo_documentation; parameters } in
          signature :: acc)
        ~init:[]
    in
    Some { signatures; activeSignature = 0; activeParameter = active_parameter }

let flow_completion_item_to_lsp
    ?token
    ~is_snippet_supported:(_ : bool)
    ~(is_preselect_supported : bool)
    ~(is_label_detail_supported : bool)
    (item : ServerProt.Response.Completion.completion_item) : Lsp.Completion.completionItem =
  let open ServerProt.Response.Completion in
  let detail =
    let trunc n s =
      if String.length s < n then
        s
      else
        String.sub s 0 n ^ "..."
    in
    let column_width = 80 in
    Some (trunc column_width item.detail)
  in
  let insertTextFormat = Some Lsp.Completion.PlainText in
  let textEdits =
    Base.List.map
      ~f:(fun (loc, newText) -> { Lsp.TextEdit.range = loc_to_lsp_range loc; newText })
      item.text_edits
  in
  let documentation = Base.Option.map item.documentation ~f:(fun doc -> [Lsp.MarkedString doc]) in
  let command =
    Some
      Lsp.Command.
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
                    ("completion", JSON_String item.name);
                  ];
              ]
            ;
        }
      
  in

  let labelDetails =
    if
      is_label_detail_supported
      && (Base.Option.is_some item.source || Base.Option.is_some item.type_)
    then
      Some
        {
          Lsp.CompletionItemLabelDetails.qualifier = item.source;
          parameters = None;
          type_ = item.type_;
        }
    else
      None
  in
  {
    Lsp.Completion.label = item.name;
    labelDetails;
    kind = item.kind;
    detail;
    documentation;
    preselect = is_preselect_supported && item.preselect;
    sortText = item.sort_text;
    filterText = None;
    insertText = None (* deprecated and should not be used *);
    insertTextFormat;
    textEdits;
    command;
    data = None;
  }

let flow_completions_to_lsp
    ?token
    ~(is_snippet_supported : bool)
    ~(is_preselect_supported : bool)
    ~(is_label_detail_supported : bool)
    (completions : ServerProt.Response.Completion.t) : Lsp.Completion.result =
  let { ServerProt.Response.Completion.items; is_incomplete } = completions in
  let items =
    Base.List.map
      ~f:
        (flow_completion_item_to_lsp
           ?token
           ~is_snippet_supported
           ~is_preselect_supported
           ~is_label_detail_supported
        )
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
  file_key_to_uri loc.Loc.source >>| fun uri -> { Lsp.Location.uri; range = loc_to_lsp_range loc }

let loc_to_lsp_with_default (loc : Loc.t) ~(default_uri : Lsp.DocumentUri.t) : Lsp.Location.t =
  let uri =
    match file_key_to_uri loc.Loc.source with
    | Ok uri -> uri
    | Error _ -> default_uri
  in
  { Lsp.Location.uri; range = loc_to_lsp_range loc }

let flow_edit_to_textedit (edit : Loc.t * string) : Lsp.TextEdit.t =
  let (loc, text) = edit in
  { Lsp.TextEdit.range = loc_to_lsp_range loc; newText = text }

let flow_loc_patch_to_lsp_edits (p : (Loc.t * string) list) : Lsp.TextEdit.t list =
  let convert_edit (loc, text) = { Lsp.TextEdit.range = loc_to_lsp_range loc; newText = text } in
  Base.List.map ~f:convert_edit p

(* ~, . and .. have no meaning in file urls so we don't canonicalize them *)
(* but symlinks must be canonicalized before being used in flow: *)
let lsp_DocumentIdentifier_to_flow_path textDocument =
  let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
  Sys_utils.realpath fn |> Base.Option.value ~default:fn

let position_of_document_position { Lsp.TextDocumentPositionParams.position; _ } =
  lsp_position_to_flow position

let diagnostics_of_flow_errors =
  let error_to_lsp
      ~(severity : Lsp.PublishDiagnostics.diagnosticSeverity) (error : Loc.t Errors.printable_error)
      : (Lsp.DocumentUri.t * Lsp.PublishDiagnostics.diagnostic) option =
    let error = Errors.Lsp_output.lsp_of_error error in
    match loc_to_lsp error.Errors.Lsp_output.loc with
    | Ok location ->
      let uri = location.Lsp.Location.uri in
      let related_to_lsp (loc, relatedMessage) =
        match loc_to_lsp loc with
        | Ok relatedLocation -> Some { Lsp.PublishDiagnostics.relatedLocation; relatedMessage }
        | Error _ -> None
      in
      let relatedInformation =
        Base.List.filter_map error.Errors.Lsp_output.relatedLocations ~f:related_to_lsp
      in
      Some
        ( uri,
          {
            Lsp.PublishDiagnostics.range = location.Lsp.Location.range;
            severity = Some severity;
            code = Lsp.PublishDiagnostics.StringCode error.Errors.Lsp_output.code;
            source = Some "Flow";
            message = error.Errors.Lsp_output.message;
            relatedInformation;
            relatedLocations = relatedInformation (* legacy fb extension *);
          }
        )
    | Error _ -> None
  in
  fun ~errors ~warnings ->
    let add severity error acc =
      match error_to_lsp ~severity error with
      | Some (uri, diagnostic) -> Lsp.UriMap.add ~combine:List.append uri [diagnostic] acc
      | None -> acc
    in
    Lsp.UriMap.empty
    |> Errors.ConcreteLocPrintableErrorSet.fold (add Lsp.PublishDiagnostics.Error) errors
    |> Errors.ConcreteLocPrintableErrorSet.fold (add Lsp.PublishDiagnostics.Warning) warnings
