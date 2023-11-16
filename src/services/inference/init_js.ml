(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module sets up the definitions for JavaScript globals. Eventually, this
   module should become redundant: we should be able to automatically interpret
   TypeScript type definition files for these and many other primitives. That
   said, in some cases handcoding may turn out to be necessary because the type
   system is not powerful enough to encode the invariants of a library
   function. In any case, this part of the design must be revisited in the
   future. *)

open Utils_js
module Ast = Flow_ast
module Parsing = Parsing_service_js
module Infer = Type_inference_js
module ErrorSet = Flow_error.ErrorSet

let is_ok { Parsing.parsed; _ } = not (FilenameSet.is_empty parsed)

let is_fail { Parsing.failed; _ } = fst failed <> []

type lib_result =
  | Lib_ok of {
      ast: (Loc.t, Loc.t) Ast.Program.t;
      file_sig: File_sig.t;
      tolerable_errors: File_sig.tolerable_error list;
    }
  | Lib_fail of Parsing.parse_failure
  | Lib_skip

let parse_lib_file ~reader options file =
  (* types are always allowed in lib files *)
  let force_types = true in
  (* lib files are always "use strict" *)
  let force_use_strict = true in
  (* do not parallelize *)
  let workers = None in
  try%lwt
    let lib_file = File_key.LibFile file in
    let filename_set = FilenameSet.singleton lib_file in
    let next = Parsing.next_of_filename_set (* workers *) None filename_set in
    let%lwt results =
      Parsing.parse_with_defaults ~force_types ~force_use_strict ~reader options workers next
    in
    Lwt.return
      ( if is_ok results then
        let ast = Parsing_heaps.Mutator_reader.get_ast_unsafe ~reader lib_file in
        let (file_sig, tolerable_errors) =
          Parsing_heaps.Mutator_reader.get_tolerable_file_sig_unsafe ~reader lib_file
        in
        Lib_ok { ast; file_sig; tolerable_errors }
      else if is_fail results then
        let error = List.hd (snd results.Parsing.failed) in
        Lib_fail error
      else
        Lib_skip
      )
  with
  | _ -> failwith (spf "Can't read library definitions file %s, exiting." file)

let check_lib_file ~ccx ~options mk_builtins ast =
  let lint_severities = Options.lint_severities options in
  let metadata =
    Context.(
      let metadata = metadata_of_options options in
      { metadata with checked = false }
    )
  in
  let (prog_loc, { Ast.Program.all_comments; _ }) = ast in
  let lib_file = Base.Option.value_exn (Loc.source prog_loc) in
  (* Lib files use only concrete locations, so this is not used. *)
  let aloc_table = lazy (ALoc.empty_table lib_file) in
  let resolve_require mref = Error (Reason.internal_module_name mref) in
  let cx =
    Context.make ccx metadata lib_file aloc_table resolve_require mk_builtins Context.InitLib
  in
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let (_ : (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t) =
    Infer.infer_lib_file ~lint_severities cx lib_file all_comments aloc_ast
  in
  Context.errors cx

(* process all lib files: parse, infer, and add the symbols they define
   to the builtins object.

   Note: we support overrides of definitions found earlier in the list of
   files by those of the same name found in later ones, so caller must
   preserve lib path declaration order in the (flattened) list of files
   passed.

   returns (success, parse and signature errors, exports)
*)
let load_lib_files ~ccx ~options ~reader ~validate_libdefs files =
  let%lwt (ok, errors, ordered_asts) =
    files
    |> Lwt_list.fold_left_s
         (fun (ok_acc, errors_acc, asts_acc) file ->
           let lib_file = File_key.LibFile file in
           match%lwt parse_lib_file ~reader options file with
           | Lib_ok { ast; file_sig = _; tolerable_errors } ->
             let errors =
               tolerable_errors
               |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:lib_file
             in
             let errors_acc = ErrorSet.union errors errors_acc in
             (* construct ast list in reverse override order *)
             let asts_acc = ast :: asts_acc in
             Lwt.return (ok_acc, errors_acc, asts_acc)
           | Lib_fail fail ->
             let errors =
               match fail with
               | Parsing.Uncaught_exception exn ->
                 Inference_utils.set_of_parse_exception ~source_file:lib_file exn
               | Parsing.Parse_error error ->
                 Inference_utils.set_of_parse_error ~source_file:lib_file error
               | Parsing.Docblock_errors errs ->
                 Inference_utils.set_of_docblock_errors ~source_file:lib_file errs
             in
             let errors_acc = ErrorSet.union errors errors_acc in
             Lwt.return (false, errors_acc, asts_acc)
           | Lib_skip -> Lwt.return (ok_acc, errors_acc, asts_acc))
         (true, ErrorSet.empty, [])
  in
  let (builtin_exports, master_cx, cx_opt) =
    if ok then
      let sig_opts = Type_sig_options.builtin_options options in
      let (builtins, master_cx) = Merge_js.merge_lib_files ~sig_opts ordered_asts in
      let cx_opt =
        match master_cx with
        | Context.EmptyMasterContext -> None
        | Context.NonEmptyMasterContext { builtin_leader_file_key; _ } ->
          let metadata =
            Context.(
              let metadata = metadata_of_options options in
              { metadata with checked = false }
            )
          in
          let mk_builtins = Merge_js.mk_builtins metadata master_cx in
          let cx =
            Context.make
              ccx
              metadata
              builtin_leader_file_key
              (lazy (ALoc.empty_table builtin_leader_file_key))
              (fun mref -> Error (Reason.InternalModuleName mref))
              mk_builtins
              Context.InitLib
          in
          ( if validate_libdefs then
            let errors =
              Base.List.fold ordered_asts ~init:ErrorSet.empty ~f:(fun errors ast ->
                  ErrorSet.union errors (check_lib_file ~ccx ~options mk_builtins ast)
              )
            in
            Context.reset_errors cx errors
          );
          Some cx
      in
      (Exports.of_builtins builtins, master_cx, cx_opt)
    else
      (Exports.empty, Context.EmptyMasterContext, None)
  in
  Lwt.return (ok, master_cx, cx_opt, errors, builtin_exports)

type init_result = {
  ok: bool;
  errors: ErrorSet.t FilenameMap.t;
  warnings: ErrorSet.t FilenameMap.t;
  suppressions: Error_suppressions.t;
  exports: Exports.t;
  master_cx: Context.master_context;
}

let error_set_to_filemap err_set =
  ErrorSet.fold
    (fun error map ->
      let file = Flow_error.source_file error in
      FilenameMap.update
        file
        (function
          | None -> Some (ErrorSet.singleton error)
          | Some set -> Some (ErrorSet.add error set))
        map)
    err_set
    FilenameMap.empty

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
*)
let init ~options ~reader ~validate_libdefs lib_files =
  let ccx = Context.make_ccx () in

  let%lwt (ok, master_cx, cx_opt, parse_and_sig_errors, exports) =
    load_lib_files ~ccx ~options ~reader ~validate_libdefs lib_files
  in

  let (errors, warnings, suppressions) =
    match cx_opt with
    | None -> (ErrorSet.empty, ErrorSet.empty, Error_suppressions.empty)
    | Some cx ->
      let errors = Context.errors cx in
      let suppressions = Context.error_suppressions cx in
      let severity_cover = Context.severity_cover cx in
      let include_suppressions = Context.include_suppressions cx in
      let aloc_tables = Context.aloc_tables cx in
      let (errors, warnings, suppressions) =
        Error_suppressions.filter_lints
          ~include_suppressions
          suppressions
          errors
          aloc_tables
          severity_cover
      in
      (errors, warnings, suppressions)
  in

  (* store master signature context to heap *)
  Context_heaps.add_master master_cx;

  let errors = ErrorSet.union parse_and_sig_errors errors in
  let (errors, warnings, suppressions) =
    (error_set_to_filemap errors, error_set_to_filemap warnings, suppressions)
  in

  Lwt.return { ok; errors; warnings; suppressions; exports; master_cx }
