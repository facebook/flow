(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js
open Collated_errors

let add_suppression_warnings root checked unused warnings =
  let open Flow_errors_utils in
  (* For each unused suppression, create an warning *)
  let all_locs = Error_suppressions.all_unused_locs unused in
  let warnings =
    Loc_collections.LocSet.fold
      (fun loc warnings ->
        let source_file =
          match Loc.source loc with
          | Some x -> x
          | None -> File_key.SourceFile "-"
        in
        (* In lazy mode, dependencies are modules which we typecheck not because we care about
         * them, but because something important (a focused file or a focused file's dependent)
         * needs these dependencies. Therefore, we might not typecheck a dependency's dependents.
         *
         * This means there might be an unused suppression comment warning in a dependency which
         * only shows up in lazy mode. To avoid this, we'll just avoid raising this kind of
         * warning in any dependency. *)
        if not (CheckedSet.mem_dependency source_file checked) then
          let err =
            let msg = Error_message.EUnusedSuppression loc in
            Flow_error.error_of_msg ~trace_reasons:[] ~source_file msg
            |> Flow_error.make_error_printable Fun.id ~strip_root:(Some root)
          in
          let file_warnings =
            FilenameMap.find_opt source_file warnings
            |> Base.Option.value ~default:ConcreteLocPrintableErrorSet.empty
            |> ConcreteLocPrintableErrorSet.add err
          in
          FilenameMap.add source_file file_warnings warnings
        else
          warnings)
      all_locs
      warnings
  in
  Error_suppressions.CodeLocSet.fold
    (fun (code, loc) warnings ->
      let source_file =
        match Loc.source loc with
        | Some x -> x
        | None -> File_key.SourceFile "-"
      in
      let err =
        Error_message.ECodelessSuppression (loc, code)
        |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file
        |> Flow_error.make_error_printable Fun.id ~strip_root:(Some root)
      in
      let file_warnings =
        FilenameMap.find_opt source_file warnings
        |> Base.Option.value ~default:ConcreteLocPrintableErrorSet.empty
        |> ConcreteLocPrintableErrorSet.add err
      in
      FilenameMap.add source_file file_warnings warnings)
    (Error_suppressions.universally_suppressed_codes unused)
    warnings

let collate_duplicate_providers ~update root =
  let pos = Loc.{ line = 1; column = 0 } in
  let f module_name (provider_file, provider) acc duplicate =
    let conflict = Loc.{ source = Some duplicate; start = pos; _end = pos } in
    let err =
      Error_message.EDuplicateModuleProvider { module_name; provider; conflict }
      |> Flow_error.error_of_msg ~trace_reasons:[] ~source_file:duplicate
      |> Flow_error.make_error_printable Fun.id ~strip_root:(Some root)
    in
    update provider_file duplicate err acc
  in
  let f module_name (provider_file, duplicates) acc =
    let provider = Loc.{ source = Some provider_file; start = pos; _end = pos } in
    Nel.fold_left (f module_name (provider_file, provider)) acc duplicates
  in
  SMap.fold f

let update_local_collated_errors ~reader ~options suppressions errors acc =
  let root = Options.root options in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let collated_local_errors =
    FilenameMap.fold
      (fun filename file_errs errors ->
        let file_options = Some (Options.file_options options) in
        (* It is okay to ignore suppressions here, since local errors, ie. parse
         * errors, parse exceptions and docblock errors, are insuppressible. *)
        let (file_errs, _file_suppressed, _unused) =
          Error_suppressions.filter_suppressed_errors
            ~root
            ~file_options
            ~loc_of_aloc
            suppressions
            file_errs
            ~unused:Error_suppressions.empty
        in
        FilenameMap.add filename file_errs errors)
      errors
      acc.collated_local_errors
  in
  { acc with collated_local_errors }

let update_collated_errors ~reader ~options ~checked_files ~all_suppressions errors acc =
  let root = Options.root options in
  let { local_errors; duplicate_providers; merge_errors; warnings; suppressions } = errors in
  let loc_of_aloc = Parsing_heaps.Reader_dispatcher.loc_of_aloc ~reader in
  let acc_fun filename file_errs (errors, suppressed, unused) =
    let file_options = Some (Options.file_options options) in
    let (file_errs, file_suppressed, unused) =
      Error_suppressions.filter_suppressed_errors
        ~root
        ~file_options
        ~loc_of_aloc
        (* Use all_suppressions here to account for misplaced errors. *)
        all_suppressions
        file_errs
        ~unused
    in
    let errors = FilenameMap.add filename file_errs errors in
    let suppressed = FilenameMap.add filename file_suppressed suppressed in
    (errors, suppressed, unused)
  in
  MonitorRPC.status_update ~event:ServerStatus.Collating_errors_start;
  let {
    collated_duplicate_providers_errors;
    collated_local_errors;
    collated_merge_errors;
    collated_warning_map;
    collated_suppressed_errors;
    error_state_timestamps;
  } =
    acc
  in
  let collated_duplicate_providers_errors =
    collate_duplicate_providers
      ~update:(fun provider_file duplicate err acc -> (err, provider_file, duplicate) :: acc)
      root
      duplicate_providers
      collated_duplicate_providers_errors
  in
  let (collated_local_errors, collated_suppressed_errors, unused) =
    (collated_local_errors, collated_suppressed_errors, suppressions)
    |> FilenameMap.fold acc_fun local_errors
  in
  let (collated_merge_errors, collated_suppressed_errors, unused) =
    (collated_merge_errors, collated_suppressed_errors, unused)
    |> FilenameMap.fold acc_fun merge_errors
  in
  let (warnings, collated_suppressed_errors, unused) =
    (FilenameMap.empty, collated_suppressed_errors, unused) |> FilenameMap.fold acc_fun warnings
  in
  (* Compute "unused suppression warnings" based on the suppression set that
   * emerged from the current checked set, not the entire set of suppressions. *)
  let collated_warning_map =
    add_suppression_warnings root checked_files unused warnings
    |> FilenameMap.union collated_warning_map
  in
  {
    collated_duplicate_providers_errors;
    collated_local_errors;
    collated_merge_errors;
    collated_warning_map;
    collated_suppressed_errors;
    error_state_timestamps;
  }

let get_with_separate_warnings env =
  let open ServerEnv in
  let open Flow_errors_utils in
  let { collated_errors; _ } = env in
  let {
    collated_duplicate_providers_errors;
    collated_local_errors;
    collated_merge_errors;
    collated_warning_map;
    collated_suppressed_errors;
    error_state_timestamps = _;
  } =
    collated_errors
  in
  let collated_errorset =
    Base.List.fold
      ~f:(fun acc (err, _, _) -> ConcreteLocPrintableErrorSet.add err acc)
      ~init:ConcreteLocPrintableErrorSet.empty
      collated_duplicate_providers_errors
    |> FilenameMap.fold (fun _ -> ConcreteLocPrintableErrorSet.union) collated_local_errors
    |> FilenameMap.fold (fun _ -> ConcreteLocPrintableErrorSet.union) collated_merge_errors
  in
  let collated_suppressed_errors =
    FilenameMap.fold (fun _ -> List.rev_append) collated_suppressed_errors []
  in
  (collated_errorset, collated_warning_map, collated_suppressed_errors)

let type_error_stat collated_errors =
  let open Flow_errors_utils in
  let {
    collated_duplicate_providers_errors = _;
    collated_local_errors = _;
    collated_merge_errors;
    collated_warning_map = _;
    collated_suppressed_errors = _;
    error_state_timestamps = _;
  } =
    collated_errors
  in
  let ( have_type_errors,
        all_type_errors_in_one_file,
        have_subtyping_errors,
        all_subtyping_errors_in_one_file
      ) =
    FilenameMap.fold
      (fun _
           errors
           ( have_type_errors_before,
             all_errors_in_one_file,
             have_subtyping_errors_before,
             all_subtyping_errors_in_one_file
           ) ->
        let have_type_errors_in_file = not (ConcreteLocPrintableErrorSet.is_empty errors) in
        let all_type_errors_in_one_file =
          all_errors_in_one_file && not (have_type_errors_before && have_type_errors_in_file)
        in
        let have_subtyping_errors_in_file =
          ConcreteLocPrintableErrorSet.exists
            (fun error ->
              match code_of_printable_error error with
              | Some
                  Error_codes.(
                    ( IncompatibleCall | IncompatibleCast | IncompatibleExact | IncompatibleExtend
                    | IncompatibleFunctionIndexer | IncompatibleIndexer | IncompatibleReturn
                    | IncompatibleType | IncompatibleTypeArg | IncompatibleTypeGuard
                    | IncompatibleUse | IncompatibleVariance | PropMissing )) ->
                true
              | _ -> false)
            errors
        in
        let all_subtyping_errors_in_one_file =
          all_subtyping_errors_in_one_file
          && not (have_subtyping_errors_before && have_subtyping_errors_in_file)
        in
        ( have_type_errors_before || have_type_errors_in_file,
          all_type_errors_in_one_file,
          have_subtyping_errors_before || have_subtyping_errors_in_file,
          all_subtyping_errors_in_one_file
        ))
      collated_merge_errors
      (false, true, false, true)
  in
  let have_type_errors_and_all_in_one_file = have_type_errors && all_type_errors_in_one_file in
  let have_subtyping_errors_and_all_in_one_file =
    have_subtyping_errors && all_subtyping_errors_in_one_file
  in
  ( have_type_errors,
    have_type_errors_and_all_in_one_file,
    have_subtyping_errors,
    have_subtyping_errors_and_all_in_one_file
  )

type error_resolution_stat = {
  time_to_resolve_all_type_errors: float option;
  time_to_resolve_all_type_errors_in_one_file: float option;
  time_to_resolve_all_subtyping_errors: float option;
  time_to_resolve_all_subtyping_errors_in_one_file: float option;
}

let update_error_state_timestamps collated_errors =
  let current_time = Unix.gettimeofday () in
  let init_timestamps = collated_errors.error_state_timestamps in
  let ( have_type_errors,
        have_type_errors_and_all_in_one_file,
        have_subtyping_errors,
        have_subtyping_errors_and_all_in_one_file
      ) =
    type_error_stat collated_errors
  in
  let timestamp_start_of_non_zero_type_errors =
    if have_type_errors then
      Base.Option.first_some
        init_timestamps.timestamp_start_of_non_zero_type_errors
        (Some current_time)
    else
      None
  in
  let timestamp_start_of_non_zero_type_errors_all_in_one_file =
    if have_type_errors_and_all_in_one_file then
      Base.Option.first_some
        init_timestamps.timestamp_start_of_non_zero_type_errors_all_in_one_file
        (Some current_time)
    else
      None
  in
  let timestamp_start_of_non_zero_subtyping_errors =
    if have_subtyping_errors then
      Base.Option.first_some
        init_timestamps.timestamp_start_of_non_zero_subtyping_errors
        (Some current_time)
    else
      None
  in
  let timestamp_start_of_non_zero_subtyping_errors_all_in_one_file =
    if have_subtyping_errors_and_all_in_one_file then
      Base.Option.first_some
        init_timestamps.timestamp_start_of_non_zero_subtyping_errors_all_in_one_file
        (Some current_time)
    else
      None
  in
  let time_to_resolve_errors init_error_time current_error_time =
    match (init_error_time, current_error_time) with
    (* If we start with no errors, then we are not resolving any errors.  *)
    | (None, _) -> None
    (* If we end with errors, then we are not fully resolving all errors. *)
    | (_, Some _) -> None
    | (Some t_start, None) -> Some (current_time -. t_start)
  in
  let time_to_resolve_all_type_errors =
    time_to_resolve_errors
      init_timestamps.timestamp_start_of_non_zero_type_errors
      timestamp_start_of_non_zero_type_errors
  in
  let time_to_resolve_all_type_errors_in_one_file =
    time_to_resolve_errors
      init_timestamps.timestamp_start_of_non_zero_type_errors_all_in_one_file
      timestamp_start_of_non_zero_type_errors
  in
  let time_to_resolve_all_subtyping_errors =
    time_to_resolve_errors
      init_timestamps.timestamp_start_of_non_zero_subtyping_errors
      timestamp_start_of_non_zero_subtyping_errors
  in
  let time_to_resolve_all_subtyping_errors_in_one_file =
    time_to_resolve_errors
      init_timestamps.timestamp_start_of_non_zero_subtyping_errors_all_in_one_file
      timestamp_start_of_non_zero_subtyping_errors
  in
  ( {
      collated_errors with
      Collated_errors.error_state_timestamps =
        {
          timestamp_start_of_non_zero_type_errors;
          timestamp_start_of_non_zero_type_errors_all_in_one_file;
          timestamp_start_of_non_zero_subtyping_errors;
          timestamp_start_of_non_zero_subtyping_errors_all_in_one_file;
        };
    },
    {
      time_to_resolve_all_type_errors;
      time_to_resolve_all_type_errors_in_one_file;
      time_to_resolve_all_subtyping_errors;
      time_to_resolve_all_subtyping_errors_in_one_file;
    }
  )

(* combine error maps into a single error set and a single warning set *)
let get env =
  let open Flow_errors_utils in
  let (errors, warning_map, suppressed_errors) = get_with_separate_warnings env in
  let warnings =
    FilenameMap.fold
      (fun _key -> ConcreteLocPrintableErrorSet.union)
      warning_map
      ConcreteLocPrintableErrorSet.empty
  in
  (errors, warnings, suppressed_errors)
