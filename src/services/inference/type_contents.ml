(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Types_js_types

type parse_contents_return =
  | Parsed of parse_artifacts  (** Note that there may be parse errors *)
  | Skipped

(* This puts a nicer interface for do_parse. At some point, `do_parse` itself should be
 * rethought, at which point `parse_contents` could call it directly without confusion. This would
 * also benefit the other callers of `do_parse`. In the meantime, this function provides the
 * interface we would like here. *)
let do_parse_wrapper ~options filename contents =
  let types_mode =
    if Options.all options then
      Parsing_service_js.TypesAllowed
    else
      Parsing_service_js.TypesForbiddenByDefault
  in
  let max_tokens = Options.max_header_tokens options in
  let (docblock_errors, docblock) =
    Parsing_service_js.parse_docblock ~max_tokens filename contents
  in
  let parse_options = Parsing_service_js.make_parse_options ~types_mode docblock options in
  let parse_result = Parsing_service_js.do_parse ~parse_options ~info:docblock contents filename in
  match parse_result with
  | Parsing_service_js.Parse_ok { ast; file_sig; tolerable_errors; _ } ->
    Parsed
      (Parse_artifacts
         { docblock; docblock_errors; ast; file_sig; tolerable_errors; parse_errors = [] }
      )
  | Parsing_service_js.Parse_recovered { ast; file_sig; tolerable_errors; parse_errors; _ } ->
    Parsed
      (Parse_artifacts
         {
           docblock;
           docblock_errors;
           ast;
           file_sig;
           tolerable_errors;
           parse_errors = Nel.to_list parse_errors;
         }
      )
  | Parsing_service_js.Parse_exn exn ->
    (* we have historically just blown up here, so we will continue to do so. *)
    Exception.reraise exn
  | Parsing_service_js.(Parse_skip (Skip_non_flow_file | Skip_resource_file | Skip_package_json _))
    ->
    (* This happens when a non-source file is queried, such as a json file *)
    Skipped

let with_timer ~options timer profiling f =
  let should_print = Options.should_profile options in
  Profiling_js.with_timer profiling ~should_print ~timer ~f

let parse_contents ~options ~profiling contents filename =
  with_timer ~options "Parsing" profiling (fun () ->
      match do_parse_wrapper ~options filename contents with
      | Parsed (Parse_artifacts { parse_errors; docblock_errors; _ } as parse_artifacts) ->
        let errors =
          match parse_errors with
          | first_parse_error :: _ ->
            let errors =
              Inference_utils.set_of_docblock_errors ~source_file:filename docblock_errors
            in
            let err =
              Inference_utils.error_of_parse_error ~source_file:filename first_parse_error
            in
            Flow_error.ErrorSet.add err errors
          | _ -> Flow_error.ErrorSet.empty
        in
        (Some parse_artifacts, errors)
      | Skipped -> (None, Flow_error.ErrorSet.empty)
  )

let errors_of_file_artifacts ~options ~env ~loc_of_aloc ~filename ~file_artifacts =
  (* Callers have already had a chance to inspect parse errors, so they are not included here.
   * Typically, type errors in the face of parse errors are meaningless, so callers should probably
   * not call this function if parse errors have been found. *)
  (* TODO consider asserting that there are no parse errors. *)
  let (Parse_artifacts { docblock_errors; tolerable_errors; _ }, Typecheck_artifacts { cx; _ }) =
    file_artifacts
  in
  let errors = Context.errors cx in
  let errors =
    tolerable_errors
    |> File_sig.abstractify_tolerable_errors
    |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:filename
    |> Flow_error.ErrorSet.union errors
  in
  let errors =
    docblock_errors
    |> Inference_utils.set_of_docblock_errors ~source_file:filename
    |> Flow_error.ErrorSet.union errors
  in
  (* Suppressions for errors in this file can come from dependencies *)
  let suppressions =
    ServerEnv.(
      let new_suppressions = Context.error_suppressions cx in
      let { suppressions; _ } = env.errors in
      Error_suppressions.update_suppressions suppressions new_suppressions
    )
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
    errors |> Flow_error.concretize_errors loc_of_aloc |> Flow_error.make_errors_printable
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
let unchecked_dependencies ~options ~reader file file_sig =
  let node_modules_containers = !Files.node_modules_containers in
  let resolved_requires =
    let require_loc_map = File_sig.With_Loc.(require_loc_map file_sig.module_sig) in
    let reader = Abstract_state_reader.State_reader reader in
    SMap.fold
      (fun r _locs acc ->
        match Module_js.imported_module ~options ~reader ~node_modules_containers file r with
        | Ok m -> Modulename.Set.add m acc
        | Error _ -> acc)
      require_loc_map
      Modulename.Set.empty
  in
  let unchecked_dependency m =
    let ( let* ) = Option.bind in
    let* file = Parsing_heaps.Reader.get_provider ~reader m in
    let* parse = Parsing_heaps.Reader.get_typed_parse ~reader file in
    match Parsing_heaps.Reader.get_leader ~reader parse with
    | None -> Some (Parsing_heaps.read_file_key file)
    | Some _ -> None
  in
  Modulename.Set.fold
    (fun m acc ->
      match unchecked_dependency m with
      | Some f -> FilenameSet.add f acc
      | None -> acc)
    resolved_requires
    FilenameSet.empty

(** Ensures that dependencies are checked; schedules them to be checked and cancels the
    Lwt thread to abort the command if not.

    This is necessary because [merge_contents] needs all of the dep type sigs to be
    available, but since it doesn't use workers it can't go parse everything itself. *)
let ensure_checked_dependencies ~options ~reader file file_sig =
  let unchecked_deps = unchecked_dependencies ~options ~reader file file_sig in
  if FilenameSet.is_empty unchecked_deps then
    ()
  else
    let n = FilenameSet.cardinal unchecked_deps in
    Hh_logger.info "Canceling command due to %d unchecked dependencies" n;
    let _ =
      FilenameSet.fold
        (fun f i ->
          let cap = 10 in
          if i <= cap then
            Hh_logger.info "%d/%d: %s" i n (File_key.to_string f)
          else if Hh_logger.Level.(passes_min_level Debug) then
            Hh_logger.debug "%d/%d: %s" i n (File_key.to_string f)
          else if i = cap + 1 then
            Hh_logger.info "..."
          else
            ();
          i + 1)
        unchecked_deps
        1
    in
    ServerMonitorListenerState.push_dependencies_to_prioritize unchecked_deps;
    raise Lwt.Canceled

(** TODO: handle case when file+contents don't agree with file system state **)
let merge_contents ~options ~profiling ~reader filename info ast file_sig =
  with_timer ~options "MergeContents" profiling (fun () ->
      let () = ensure_checked_dependencies ~options ~reader filename file_sig in
      Merge_service.check_contents_context ~reader options filename ast info file_sig
  )

let type_parse_artifacts ~options ~profiling filename intermediate_result =
  match intermediate_result with
  | (Some (Parse_artifacts { docblock; ast; file_sig; _ } as parse_artifacts), _errs) ->
    (* We assume that callers have already inspected the parse errors, so we discard them here. *)
    let reader = State_reader.create () in
    let (cx, typed_ast) =
      merge_contents ~options ~profiling ~reader filename docblock ast file_sig
    in
    Ok (parse_artifacts, Typecheck_artifacts { cx; typed_ast })
  | (None, errs) -> Error errs
