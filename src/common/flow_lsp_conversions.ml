(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let flow_completion_to_lsp
    (item: ServerProt.Response.complete_autocomplete_result)
  : Lsp.Completion.completionItem =
  let open Lsp.Completion in
  let open ServerProt.Response in
  let trunc n s = if String.length s < n then s else (String.sub s 0 n) ^ "..." in
  let trunc80 s = trunc 80 s in
  let flow_params_to_string params =
    let params = List.map (fun p -> p.param_name ^ ": " ^ p.param_ty) params in
    "(" ^ (String.concat ", " params) ^ ")"
  in
  let kind, itemType, inlineDetail, detail = match item.func_details with
    | Some func_details ->
      let kind = Some Function in
      let itemType = Some (trunc 30 func_details.return_ty) in
      let inlineDetail = Some (trunc 40 (flow_params_to_string func_details.param_tys)) in
      let detail = Some (trunc80 item.res_ty) in
      kind, itemType, inlineDetail, detail
    | None ->
      let kind = None in
      let itemType = None in
      let inlineDetail = Some (trunc80 item.res_ty) in
      let detail = Some (trunc80 item.res_ty) in
      kind, itemType, inlineDetail, detail
  in
  {
    label = item.res_name;
    kind;
    detail = detail;
    inlineDetail;
    itemType;
    documentation = None; (* This will be filled in by completionItem/resolve. *)
    sortText = None;
    filterText = None;
    insertText = None;
    insertTextFormat = Some PlainText;
    textEdits = [];
    command = None;
    data = None;
  }

let file_key_to_uri (file_key_opt: File_key.t option): (string, string) result =
  let (>>|) = Core_result.(>>|) in
  let (>>=) = Core_result.(>>=) in
  Core_result.of_option file_key_opt ~error:"File_key is None"
    >>= File_key.to_path
    >>| File_url.create

let loc_to_lsp_range (loc: Loc.t): Lsp.range =
  { Lsp.
    start = { Lsp.line=loc.Loc.start.Loc.line-1; character=loc.Loc.start.Loc.column; };
    end_ = { Lsp.line=loc.Loc._end.Loc.line-1; character=loc.Loc._end.Loc.column; };
  }

let loc_to_lsp (loc: Loc.t): (Lsp.Location.t, string) result =
  let (>>|) = Core_result.(>>|) in
  file_key_to_uri loc.Loc.source >>| fun uri -> { Lsp.Location.uri; range = loc_to_lsp_range loc; }

let loc_to_lsp_with_default (loc: Loc.t) ~(default_uri: string): Lsp.Location.t =
  let uri = match file_key_to_uri loc.Loc.source with
    | Ok uri -> uri
    | Error _ -> default_uri
  in
  { Lsp.Location.uri; range = loc_to_lsp_range loc; }

let lsp_position_to_flow (position: Lsp.position): int * int =
  let open Lsp in
  let line = position.line + 1 in
  let char = position.character
  in
  (line, char)

let lsp_DocumentIdentifier_to_flow
    (textDocument: Lsp.TextDocumentIdentifier.t)
    ~(client: Persistent_connection.single_client)
  : File_input.t =
  let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
  (* ~, . and .. have no meaning in file urls so we don't canonicalize them *)
  (* but symlinks must be canonicalized before being used in flow: *)
  let fn = Option.value (Sys_utils.realpath fn) ~default:fn in
  let file = Persistent_connection.get_file client fn
  in
  file

let lsp_DocumentPosition_to_flow
    (params: Lsp.TextDocumentPositionParams.t)
    ~(client: Persistent_connection.single_client)
  : File_input.t * int * int =
  let open Lsp.TextDocumentPositionParams in
  let file = lsp_DocumentIdentifier_to_flow params.textDocument client in
  let (line, char) = lsp_position_to_flow params.position
  in
  (file, line, char)
