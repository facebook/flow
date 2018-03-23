(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let error_of_docblock_error ~source_file (loc, err) =
  let flow_err = Flow_error.EDocblockError (loc, match err with
    | Parsing_service_js.MultipleFlowAttributes -> Flow_error.MultipleFlowAttributes
    | Parsing_service_js.MultipleProvidesModuleAttributes -> Flow_error.MultipleProvidesModuleAttributes
    | Parsing_service_js.MultipleJSXAttributes -> Flow_error.MultipleJSXAttributes
    | Parsing_service_js.InvalidJSXAttribute first_error -> Flow_error.InvalidJSXAttribute first_error
  ) in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_docblock_errors ~source_file errors =
  List.fold_left (fun acc err ->
    Errors.ErrorSet.add (error_of_docblock_error ~source_file err) acc
  ) Errors.ErrorSet.empty errors

let error_of_parse_error ~source_file (loc, err) =
  let flow_err = Flow_error.EParseError (loc, err) in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_parse_error ~source_file error =
  Errors.ErrorSet.singleton (error_of_parse_error ~source_file error)

let error_of_file_sig_error ~source_file err =
  let flow_err = match err with
  | File_sig.IndeterminateModuleType loc -> Flow_error.EIndeterminateModuleType loc
  in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_file_sig_error ~source_file error =
  Errors.ErrorSet.singleton (error_of_file_sig_error ~source_file error)

let error_of_file_sig_tolerable_error ~source_file err =
  let flow_err = match err with
  | File_sig.BadExportPosition loc -> Flow_error.EBadExportPosition loc
  in
  Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err

let set_of_file_sig_tolerable_errors ~source_file errors =
  errors
  |> List.map (error_of_file_sig_tolerable_error ~source_file)
  |> Errors.ErrorSet.of_list
