(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        | Parsing_service_js.MultipleProvidesModuleAttributes ->
          Error_message.MultipleProvidesModuleAttributes
        | Parsing_service_js.MultipleJSXAttributes -> Error_message.MultipleJSXAttributes
        | Parsing_service_js.InvalidJSXAttribute first_error ->
          Error_message.InvalidJSXAttribute first_error )
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

let error_of_package_json_error ~source_file (loc, err) =
  Error_message.EMalformedPackageJson (ALoc.of_loc loc, err)
  |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file

let set_of_package_json_error ~source_file =
  error_of_package_json_error ~source_file %> Flow_error.ErrorSet.singleton

let error_of_file_sig_error ~source_file err =
  File_sig.With_Loc.(
    let flow_err =
      match err with
      | IndeterminateModuleType loc -> Error_message.EIndeterminateModuleType (ALoc.of_loc loc)
    in
    Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err)

let set_of_file_sig_error ~source_file =
  error_of_file_sig_error ~source_file %> Flow_error.ErrorSet.singleton

let error_of_file_sig_tolerable_error ~source_file err =
  File_sig.With_ALoc.(
    let flow_err =
      match err with
      | BadExportPosition loc -> Error_message.EBadExportPosition loc
      | BadExportContext (name, loc) -> Error_message.EBadExportContext (name, loc)
      | SignatureVerificationError sve -> Error_message.ESignatureVerification sve
    in
    Flow_error.error_of_msg ~trace_reasons:[] ~source_file flow_err)

let set_of_file_sig_tolerable_errors ~source_file =
  Base.List.map ~f:(error_of_file_sig_tolerable_error ~source_file) %> Flow_error.ErrorSet.of_list

(* This is an options-aware fold over the files in `m`. Function `f` will be applied
 * to a file FILE in `m` iff:
 *  - flag 'opt_enforce_well_formed_exports' is set to true, and
 *  - if at least one 'opt_enforce_well_formed_exports_includes=PATH' has been
 *    set, then there exists PATH for which FILE is within PATH.
 *)
let fold_included_well_formed_exports ~f options m acc =
  if not options.Options.opt_enforce_well_formed_exports then
    acc
  else
    match options.Options.opt_enforce_well_formed_exports_includes with
    | [] -> Utils_js.FilenameMap.fold f m acc
    | paths ->
      Utils_js.FilenameMap.fold
        (fun file v b ->
          let file_str = File_key.to_string file |> Sys_utils.normalize_filename_dir_sep in
          if List.exists (fun r -> String_utils.is_substring r file_str) paths then
            f file v b
          else
            b)
        m
        acc

let well_formed_exports_enabled options file =
  options.Options.opt_enforce_well_formed_exports
  &&
  match options.Options.opt_enforce_well_formed_exports_includes with
  | [] -> true
  | paths ->
    let file_str = File_key.to_string file |> Sys_utils.normalize_filename_dir_sep in
    List.exists (fun r -> String_utils.is_substring r file_str) paths

(* Iterate over the files in `ctx_nel`, applying function `f` to a file iff:
 *  - Flowconfig flag 'strict_es6_import_export' is set to true, and
 *  - There does not exist an 'strict_es6_import_export_excludes=PATH' for which
 *    the file is within PATH
 *)
let iter_strict_es6_import_export ~f options ctx_nel =
  if Options.strict_es6_import_export options then
    let excludes =
      Base.List.map ~f:Str.regexp options.Options.opt_strict_es6_import_export_excludes
    in
    Nel.iter
      (fun (ctx, ast, _) ->
        let file_key = Context.file ctx in
        let file_str = File_key.to_string file_key |> Sys_utils.normalize_filename_dir_sep in
        let excluded = Base.List.exists ~f:(fun r -> Str.string_match r file_str 0) excludes in
        if not excluded then f ast)
      ctx_nel
