(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Types_js_types

type parse_contents_return =
  | Parsed of parse_artifacts  (** Note that there may be parse errors *)
  | Skipped
  | File_sig_error of File_sig.With_Loc.error
      (** These errors are currently fatal to the parse. It would be nice to make them not fatal, at
       * which point this whole type could be replaced by just `parse_artifacts option` *)
  | Docblock_errors of Parsing_service_js.docblock_error list
      (** Normally these are included in `Parse_artifacts` since they do not prevent us from
       * parsing. However, for consistency with `flow status` and `flow check`, we return docblock
       * errors instead of the file sig error if we encounter a file sig error but have previously
       * encountered docblock errors. We could eliminate this case by changing the behavior of the
       * main error-checking code, or by making file sig errors non-fatal to the parse. *)

(* This puts a nicer interface for do_parse. At some point, `do_parse` itself should be
 * rethought, at which point `parse_contents` could call it directly without confusion. This would
 * also benefit the other callers of `do_parse`. In the meantime, this function provides the
 * interface we would like here. *)
let do_parse_wrapper ~options filename contents =
  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let max_tokens = Options.max_header_tokens options in
  let (docblock_errors, docblock) =
    Parsing_service_js.parse_docblock ~max_tokens filename contents
  in
  let parse_options =
    Parsing_service_js.make_parse_options ~fail:false ~types_mode docblock options
  in
  let parse_result = Parsing_service_js.do_parse ~info:docblock ~parse_options contents filename in
  (* override docblock info *)
  let docblock = Docblock.set_flow_mode_for_ide_command docblock in
  match parse_result with
  | Parsing_service_js.Parse_ok { ast; file_sig; tolerable_errors; parse_errors; _ } ->
    Parsed
      (Parse_artifacts { docblock; docblock_errors; ast; file_sig; tolerable_errors; parse_errors })
  | Parsing_service_js.Parse_fail fails ->
    let errors =
      match fails with
      | Parsing_service_js.Parse_error _ ->
        (* We pass `~fail:false` to `do_parse` above, so we should never reach this case. *)
        failwith "Unexpectedly encountered Parse_fail with parse errors"
      | Parsing_service_js.Docblock_errors _ ->
        (* Parsing_service_js.do_parse cannot create these. They are only created by another
         * caller of do_parse. It would be nice to prove this fact via the type system. *)
        failwith "Unexpectedly encountered docblock errors"
      | Parsing_service_js.File_sig_error err ->
        begin
          match docblock_errors with
          | [] ->
            (* Even with `~fail:false`, `do_parse` cannot currently recover from file sig errors, so
               * we must handle them here. *)
            File_sig_error err
          | _ ->
            (* See comments on parse_contents_return type for an explanation of this behavior *)
            Docblock_errors docblock_errors
        end
    in
    errors
  | Parsing_service_js.(Parse_skip (Skip_non_flow_file | Skip_resource_file | Skip_package_json _))
    ->
    (* This happens when a non-source file is queried, such as a json file *)
    Skipped

let parse_contents ~options ~profiling contents filename =
  let%lwt parse_result =
    Memory_utils.with_memory_timer_lwt ~options "Parsing" profiling (fun () ->
        Lwt.return (do_parse_wrapper ~options filename contents))
  in
  match parse_result with
  | Parsed (Parse_artifacts { parse_errors; docblock_errors; _ } as parse_artifacts) ->
    let errors =
      match parse_errors with
      | first_parse_error :: _ ->
        let errors = Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors in
        let err = Inference_utils.error_of_parse_error ~source_file:filename first_parse_error in
        Flow_error.ErrorSet.add err errors
      | _ -> Flow_error.ErrorSet.empty
    in
    Lwt.return (Some parse_artifacts, errors)
  | Skipped -> Lwt.return (None, Flow_error.ErrorSet.empty)
  | Docblock_errors errs ->
    let errs = Inference_utils.set_of_docblock_errors ~source_file:filename errs in
    Lwt.return (None, errs)
  | File_sig_error err ->
    let err = Inference_utils.error_of_file_sig_error ~source_file:filename err in
    let errs = Flow_error.ErrorSet.singleton err in
    Lwt.return (None, errs)

let errors_of_file_artifacts ~options ~env ~loc_of_aloc ~filename ~file_artifacts =
  (* Callers have already had a chance to inspect parse errors, so they are not included here.
   * Typically, type errors in the face of parse errors are meaningless, so callers should probably
   * not call this function if parse errors have been found. *)
  (* TODO consider asserting that there are no parse errors. *)
  let (Parse_artifacts { docblock_errors; tolerable_errors; _ }, Typecheck_artifacts { cx; _ }) =
    file_artifacts
  in
  let errors = Context.errors cx in
  let local_errors =
    tolerable_errors
    |> File_sig.abstractify_tolerable_errors
    |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:filename
  in
  let docblock_errors =
    Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors
  in
  (* Suppressions for errors in this file can come from dependencies *)
  let suppressions =
    ServerEnv.(
      let new_suppressions = Context.error_suppressions cx in
      let { suppressions; _ } = env.errors in
      Error_suppressions.update_suppressions suppressions new_suppressions)
  in
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
  let errors =
    errors
    |> Flow_error.ErrorSet.union local_errors
    |> Flow_error.ErrorSet.union docblock_errors
    |> Flow_error.concretize_errors loc_of_aloc
    |> Flow_error.make_errors_printable
  in
  let warnings =
    warnings |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
  in
  let root = Options.root options in
  let file_options = Some (Options.file_options options) in
  (* Filter out suppressed errors *)
  let (errors, _, _) =
    Error_suppressions.filter_suppressed_errors
      ~root
      ~file_options
      suppressions
      errors
      ~unused:Error_suppressions.empty
    (* TODO: track unused suppressions *)
  in
  (* Filter out suppressed warnings *)
  let (warnings, _, _) =
    Error_suppressions.filter_suppressed_errors
      ~root
      ~file_options
      suppressions
      warnings
      ~unused:Error_suppressions.empty
    (* TODO: track unused suppressions *)
  in
  let warnings =
    if Options.should_include_warnings options then
      warnings
    else
      Errors.ConcreteLocPrintableErrorSet.empty
  in
  (errors, warnings)

let printable_errors_of_file_artifacts_result ~options ~env filename result =
  let reader = State_reader.create () in
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  match result with
  | Ok file_artifacts ->
    let (errors, warnings) =
      errors_of_file_artifacts ~options ~env ~loc_of_aloc ~filename ~file_artifacts
    in
    (errors, warnings)
  | Error errors ->
    let errors =
      errors |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
    in
    (errors, Errors.ConcreteLocPrintableErrorSet.empty)

(** Resolves dependencies of [file_sig] specifically for checking contents, rather than
    for persisting in the heap. Notably, does not error if a required module is not found. *)
let resolved_requires_of_contents ~options ~reader ~env file file_sig =
  let audit = Expensive.warn in
  let reader = Abstract_state_reader.State_reader reader in
  let node_modules_containers = !Files.node_modules_containers in
  let resolved_requires =
    let require_loc_map = File_sig.With_Loc.(require_loc_map file_sig.module_sig) in
    SMap.fold
      (fun r locs resolved_rs ->
        let loc = Nel.hd locs |> ALoc.of_loc in
        let resolved_r =
          Module_js.imported_module ~options ~reader ~node_modules_containers file loc r
        in
        Modulename.Set.add resolved_r resolved_rs)
      require_loc_map
      Modulename.Set.empty
  in
  let is_checked f =
    FilenameSet.mem f env.ServerEnv.files && Module_js.checked_file ~reader f ~audit
  in
  Modulename.Set.fold
    (fun m acc ->
      match Module_heaps.Reader_dispatcher.get_file ~reader m ~audit with
      | Some f ->
        if is_checked f then
          FilenameSet.add f acc
        else
          acc
      | None -> acc) (* complain elsewhere about required module not found *)
    resolved_requires
    FilenameSet.empty

(** When checking contents, ensure that dependencies are checked. Might have more
    general utility. *)
let ensure_checked_dependencies ~options ~reader ~env file file_sig =
  let resolved_requires = resolved_requires_of_contents ~options ~reader ~env file file_sig in
  let unchecked_dependencies =
    let all_deps = CheckedSet.add ~dependencies:resolved_requires CheckedSet.empty in
    CheckedSet.diff all_deps env.ServerEnv.checked_files
  in

  (* Often, all dependencies have already been checked, so input contains no unchecked files.
   * In that case, let's short-circuit typecheck, since a no-op typecheck still takes time on
   * large repos *)
  if CheckedSet.is_empty unchecked_dependencies then
    Lwt.return_unit
  else (
    Hh_logger.info
      "Canceling command due to %d unchecked dependencies"
      (CheckedSet.cardinal unchecked_dependencies);
    let reason = LspProt.Unchecked_dependencies { filename = File_key.to_string file } in
    ServerMonitorListenerState.push_checked_set_to_force ~reason unchecked_dependencies;
    raise Lwt.Canceled
  )

(** TODO: handle case when file+contents don't agree with file system state **)
let merge_contents ~options ~env ~reader filename info ast file_sig =
  let%lwt () = ensure_checked_dependencies ~options ~reader ~env filename file_sig in
  Lwt.return (Merge_service.check_contents_context ~reader options filename ast info file_sig)

let file_artifacts_of_parse_artifacts ~options ~env ~reader ~profiling ~filename ~parse_artifacts =
  let (Parse_artifacts { docblock; ast; file_sig; _ }) = parse_artifacts in
  let%lwt (cx, typed_ast) =
    Memory_utils.with_memory_timer_lwt ~options "MergeContents" profiling (fun () ->
        merge_contents ~options ~env ~reader filename docblock ast file_sig)
  in
  Lwt.return (parse_artifacts, Typecheck_artifacts { cx; typed_ast })

let type_parse_artifacts ~options ~env ~profiling filename intermediate_result =
  match intermediate_result with
  | (Some parse_artifacts, _errs) ->
    (* We assume that callers have already inspected the parse errors, so we discard them here. *)
    let reader = State_reader.create () in
    let%lwt file_artifacts =
      file_artifacts_of_parse_artifacts ~options ~env ~reader ~profiling ~filename ~parse_artifacts
    in
    Lwt.return (Ok file_artifacts)
  | (None, errs) -> Lwt.return (Error errs)
