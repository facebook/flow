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
        | Docblock_parser.MultipleFlowAttributes ->
          Flow_intermediate_error_types.MultipleFlowAttributes
        | Docblock_parser.InvalidFlowMode s -> Flow_intermediate_error_types.InvalidFlowMode s
        | Docblock_parser.MultipleJSXAttributes ->
          Flow_intermediate_error_types.MultipleJSXAttributes
        | Docblock_parser.InvalidJSXAttribute first_error ->
          Flow_intermediate_error_types.InvalidJSXAttribute first_error
        | Docblock_parser.MultipleJSXRuntimeAttributes ->
          Flow_intermediate_error_types.MultipleJSXRuntimeAttributes
        | Docblock_parser.InvalidJSXRuntimeAttribute ->
          Flow_intermediate_error_types.InvalidJSXRuntimeAttribute
        | Docblock_parser.InvalidSupportsPlatform p ->
          Flow_intermediate_error_types.InvalidSupportsPlatform p
        | Docblock_parser.DisallowedSupportsPlatform ->
          Flow_intermediate_error_types.DisallowedSupportsPlatform
      )
  in
  Flow_error.error_of_msg ~source_file flow_err

let set_of_docblock_errors ~source_file =
  Base.List.fold_left
    ~f:(fun acc err -> Flow_error.ErrorSet.add (error_of_docblock_error ~source_file err) acc)
    ~init:Flow_error.ErrorSet.empty

let error_of_parse_error ~source_file (loc, err) =
  Error_message.EParseError (ALoc.of_loc loc, err) |> Flow_error.error_of_msg ~source_file

let set_of_parse_error ~source_file =
  error_of_parse_error ~source_file %> Flow_error.ErrorSet.singleton

let error_of_parse_exception ~source_file exn =
  let file_loc = Loc.{ none with source = Some source_file } |> ALoc.of_loc in
  Error_message.EInternal (file_loc, Error_message.ParseJobException exn)
  |> Flow_error.error_of_msg ~source_file

let set_of_parse_exception ~source_file =
  error_of_parse_exception ~source_file %> Flow_error.ErrorSet.singleton

let error_of_file_sig_tolerable_error ~source_file err =
  let open File_sig in
  let flow_err =
    let (SignatureVerificationError sve) = err in
    Error_message.ESignatureVerification (Signature_error.map ALoc.of_loc sve)
  in
  Flow_error.error_of_msg ~source_file flow_err

let set_of_file_sig_tolerable_errors ~source_file =
  Base.List.map ~f:(error_of_file_sig_tolerable_error ~source_file) %> Flow_error.ErrorSet.of_list
