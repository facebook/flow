(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

let error_of_docblock_error ~source_file (loc, err) =
  let flow_err =
    Error_message.EDocblockError
      ( ALoc.of_loc loc,
        match err with
        | Parsing_service_js.MultipleFlowAttributes -> Error_message.MultipleFlowAttributes
        | Parsing_service_js.InvalidFlowMode s -> Error_message.InvalidFlowMode s
        | Parsing_service_js.MultipleProvidesModuleAttributes ->
          Error_message.MultipleProvidesModuleAttributes
        | Parsing_service_js.MultipleJSXAttributes -> Error_message.MultipleJSXAttributes
        | Parsing_service_js.InvalidJSXAttribute first_error ->
          Error_message.InvalidJSXAttribute first_error
        | Parsing_service_js.MultipleJSXRuntimeAttributes ->
          Error_message.MultipleJSXRuntimeAttributes
        | Parsing_service_js.InvalidJSXRuntimeAttribute -> Error_message.InvalidJSXRuntimeAttribute
      )
  in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_docblock_errors ~source_file =
  Base.List.fold_left
    ~f:(fun acc err -> Flow_error.ErrorSet.add (error_of_docblock_error ~source_file err) acc)
    ~init:Flow_error.ErrorSet.empty

let error_of_parse_error ~source_file (loc, err) =
  Error_message.EParseError (ALoc.of_loc loc, err)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file

let set_of_parse_error ~source_file =
  error_of_parse_error ~source_file %> Flow_error.ErrorSet.singleton

let error_of_parse_exception ~source_file exn =
  let file_loc = Loc.{ none with source = Some source_file } |> ALoc.of_loc in
  Error_message.EInternal (file_loc, Error_message.ParseJobException exn)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file

let set_of_parse_exception ~source_file =
  error_of_parse_exception ~source_file %> Flow_error.ErrorSet.singleton

let error_of_file_sig_tolerable_error ~source_file err =
  let open File_sig.With_ALoc in
  let flow_err =
    match err with
    | IndeterminateModuleType loc -> Error_message.EIndeterminateModuleType loc
    | BadExportPosition loc -> Error_message.EBadExportPosition loc
    | BadExportContext (name, loc) -> Error_message.EBadExportContext (name, loc)
    | SignatureVerificationError sve -> Error_message.ESignatureVerification sve
  in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_file_sig_tolerable_errors ~source_file =
  Base.List.map ~f:(error_of_file_sig_tolerable_error ~source_file) %> Flow_error.ErrorSet.of_list
