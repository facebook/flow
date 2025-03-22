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
module ErrorSet = Flow_error.ErrorSet

(* process all lib files: parse, infer, and add the symbols they define
   to the builtins object.

   Note: we support overrides of definitions found earlier in the list of
   files by those of the same name found in later ones, so caller must
   preserve lib path declaration order in the (flattened) list of files
   passed.

   returns (success, parse and signature errors, exports)
*)
let load_lib_files ~ccx ~options ~reader files =
  let%lwt (ok, errors, ordered_asts) =
    files
    |> Lwt_list.fold_left_s
         (fun (ok_acc, errors_acc, asts_with_scoped_dir_opt_acc) (scoped_dir_opt, file) ->
           let lib_file = File_key.LibFile file in
           match Parsing_heaps.Mutator_reader.get_ast ~reader lib_file with
           | Some ast ->
             (* construct ast list in reverse override order *)
             let asts_acc = (scoped_dir_opt, ast) :: asts_with_scoped_dir_opt_acc in
             Lwt.return (ok_acc, errors_acc, asts_acc)
           | None ->
             Hh_logger.info "Failed to find %s in parsing heap." (File_key.show lib_file);
             Lwt.return (false, errors_acc, asts_with_scoped_dir_opt_acc))
         (true, ErrorSet.empty, [])
  in
  let (builtin_exports, master_cx, cx_opt) =
    if ok then (
      let sig_opts = Type_sig_options.builtin_options options in
      let (builtin_errors, master_cx) =
        Merge_js.merge_lib_files
          ~project_opts:(Options.projects_options options)
          ~sig_opts
          ordered_asts
      in
      match master_cx with
      | Context.EmptyMasterContext -> ((Exports.empty, []), Context.EmptyMasterContext, None)
      | Context.NonEmptyMasterContext
          { builtin_leader_file_key; unscoped_builtins; scoped_builtins; _ } ->
        let metadata =
          Context.(
            let metadata = metadata_of_options options in
            { metadata with checked = false }
          )
        in
        let cx =
          Context.make
            ccx
            metadata
            builtin_leader_file_key
            (lazy (ALoc.empty_table builtin_leader_file_key))
            (fun mref -> Context.MissingModule mref)
            (fun _ -> Builtins.empty ())
        in
        let (severity_cover, suppressions, suppression_errors) =
          Type_inference_js.scan_for_suppressions
            ~in_libdef:true
            (Options.lint_severities options)
            (Base.List.map ordered_asts ~f:(fun (_, ast) ->
                 let (loc, { Flow_ast.Program.all_comments; _ }) = ast in
                 (Base.Option.value_exn (Loc.source loc), all_comments)
             )
            )
        in
        Context.reset_errors cx builtin_errors;
        Context.add_severity_covers cx severity_cover;
        Context.add_error_suppressions cx suppressions;
        List.iter (Flow_js.add_output cx) suppression_errors;
        let exports_of_builtins (Context.BuiltinGroup { builtins; _ }) =
          Exports.of_builtins builtins
        in
        ( ( exports_of_builtins unscoped_builtins,
            Base.List.Assoc.map ~f:exports_of_builtins scoped_builtins
          ),
          master_cx,
          Some cx
        )
    ) else
      ((Exports.empty, []), Context.EmptyMasterContext, None)
  in
  Lwt.return (ok, master_cx, cx_opt, errors, builtin_exports)

type init_result = {
  ok: bool;
  errors: ErrorSet.t FilenameMap.t;
  warnings: ErrorSet.t FilenameMap.t;
  suppressions: Error_suppressions.t;
  exports: Exports.t * (Flow_projects.t * Exports.t) list;
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
let init ~options ~reader lib_files =
  let ccx = Context.make_ccx () in

  let%lwt (ok, master_cx, cx_opt, parse_and_sig_errors, exports) =
    load_lib_files ~ccx ~options ~reader lib_files
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
