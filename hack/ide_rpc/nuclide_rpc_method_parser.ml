(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Ide_message
open Ide_parser_utils
open Ide_rpc_method_parser_utils
open Ide_rpc_protocol_parser_types
open Result.Monad_infix

let get_position_filed = get_obj_field "position"

let get_line_field = get_int_field "line"

let get_column_field = get_int_field "column"

let get_contents_field = get_string_field "contents"

let get_start_field = get_obj_field "start"

let get_end_field = get_obj_field "end"

let get_changes_field = get_array_field "changes"

let parse_position position =
  get_line_field position >>= fun line ->
  get_column_field position >>= fun column ->
  Result.Ok { File_content.line; column; }

let parse_autocomplete_params params =
  get_filename_filed params >>= fun filename ->
  get_position_filed params >>= parse_position >>= fun position ->
  Result.Ok { filename; position; }

let parse_did_open_file_params params =
  get_filename_filed params >>= fun did_open_file_filename ->
  get_contents_field params >>= fun did_open_file_text ->
  Result.Ok { did_open_file_filename; did_open_file_text; }

let parse_did_close_file_params params =
  get_filename_filed params >>= fun did_close_file_filename ->
  Result.Ok { did_close_file_filename; }

let parse_range range =
  get_start_field range >>= parse_position >>= fun st ->
  get_end_field range >>= parse_position >>= fun ed ->
  Result.Ok (Some { File_content.st; ed; })

let parse_edit edit =
  get_text_field edit >>= fun text ->
  maybe_get_obj_field "range" edit >>= fun range_opt ->
  (match range_opt with
  | None -> Result.Ok None
  | Some range -> parse_range range) >>= fun range ->
  Result.Ok { File_content.text; range; }

let accumulate_edits edit acc =
  acc >>= fun acc ->
  parse_edit edit >>= fun edit ->
  Result.Ok (edit::acc)

let parse_did_change_file_params params =
  get_filename_filed params >>= fun did_change_file_filename ->
  get_changes_field params >>= List.fold_right
    ~f:accumulate_edits
    ~init:(Result.Ok []) >>= fun changes ->
  Result.Ok {
    Ide_message.did_change_file_filename;
    changes;
  }

let parse_autocomplete method_name params =
  assert_params_required method_name params >>=
  parse_autocomplete_params >>= fun params ->
  Result.Ok (Autocomplete params)

let parse_did_open_file method_name params =
  assert_params_required method_name params >>=
  parse_did_open_file_params >>= fun params ->
  Result.Ok (Did_open_file params)

let parse_did_close_file method_name params =
  assert_params_required method_name params >>=
  parse_did_close_file_params >>= fun params ->
  Result.Ok (Did_close_file params)

let parse_did_change_file method_name params =
  assert_params_required method_name params >>=
  parse_did_change_file_params >>= fun params ->
  Result.Ok (Did_change_file params)

let parse_method method_name params = match method_name with
  | "getCompletions" -> parse_autocomplete method_name params
  | "didOpenFile" -> parse_did_open_file method_name params
  | "didCloseFile" -> parse_did_close_file method_name params
  | "didChangeFile" -> parse_did_change_file method_name params
  | "notifyDiagnostics" -> Result.Ok Subscribe_diagnostics
  | "disconnect" -> Result.Ok Disconnect
  | "sleep" -> Result.Ok Sleep_for_test
  | _ -> Result.Error (Method_not_found method_name)

let parse ~method_name ~params =
  match method_name with
  | Ide_rpc_protocol_parser_types.Unsubscribe_call ->
    Result.Ok Ide_message.Unsubscribe_call
  | Method_name method_name -> parse_method method_name params
