(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is a data structure used to track what locations are being suppressed
 * and which suppressions have yet to be used.
 *)

open Severity
open Utils_js
open Loc_collections
open Suppression_comments

exception No_source of string

let file_of_loc_unsafe loc =
  match loc.Loc.source with
  | Some x -> x
  | None -> raise (No_source (Loc.debug_to_string ~include_source:true loc))

module CodeLocSet : Flow_set.S with type elt = string * Loc.t = Flow_set.Make (struct
  type t = string * Loc.t

  let compare (c1, l1) (c2, l2) =
    let k = String.compare c1 c2 in
    if k = 0 then
      Loc.compare l1 l2
    else
      k
end)

module FileSuppressions : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val add : Loc.t -> applicable_codes -> t -> t

  val remove : Loc.t -> CodeSet.t -> t -> t

  val union : t -> t -> t

  val add_lint_suppression : Loc.t -> t -> t

  val remove_lint_suppression : Loc.t -> t -> t

  val suppression_at_loc : Loc.t -> t -> (LocSet.t * applicable_codes) option

  val all_unused_locs : t -> LocSet.t

  val universally_suppressed_codes : t -> CodeLocSet.t
end = struct
  type error_suppressions = (LocSet.t * applicable_codes) SpanMap.t

  type t = {
    suppressions: error_suppressions;
    used_universal_suppressions: CodeLocSet.t SpanMap.t;
    lint_suppressions: LocSet.t;
  }

  let empty =
    {
      suppressions = SpanMap.empty;
      used_universal_suppressions = SpanMap.empty;
      lint_suppressions = LocSet.empty;
    }

  let is_empty { suppressions; lint_suppressions; _ } =
    SpanMap.is_empty suppressions && LocSet.is_empty lint_suppressions

  let add loc codes { suppressions; lint_suppressions; used_universal_suppressions } =
    let suppression_loc =
      Loc.(
        let start = { line = loc._end.line + 1; column = 0 } in
        let _end = { line = loc._end.line + 2; column = 0 } in
        { loc with start; _end }
      )
    in
    let suppressions =
      SpanMap.add
        suppression_loc
        (LocSet.singleton loc, codes)
        suppressions
        ~combine:(fun (set1, codes1) (set2, codes2) ->
          (LocSet.union set1 set2, join_applicable_codes codes1 codes2))
    in
    { suppressions; lint_suppressions; used_universal_suppressions }

  let all_unused_locs { suppressions; lint_suppressions; _ } =
    SpanMap.fold
      (fun _ (_, codes) acc ->
        let locs = Suppression_comments.locs_of_applicable_codes codes |> LocSet.of_list in
        LocSet.union locs acc)
      suppressions
      lint_suppressions

  let universally_suppressed_codes { used_universal_suppressions; _ } =
    used_universal_suppressions
    |> SpanMap.values
    |> List.fold_left CodeLocSet.union CodeLocSet.empty

  let remove loc codes { suppressions; lint_suppressions; used_universal_suppressions } =
    let supp_at_loc = SpanMap.find_opt loc suppressions in
    let (suppressions, used_universal_suppressions) =
      match supp_at_loc with
      | Some (locs, Specific codes') when CodeSet.subset codes codes' ->
        let new_codes = CodeSet.diff codes' codes in
        if CodeSet.is_empty new_codes then
          (SpanMap.remove loc suppressions, used_universal_suppressions)
        else
          let orig_loc =
            (* We don't want to overwrite the original location with one that falls inside it *)
            SpanMap.keys suppressions |> Base.List.find_exn ~f:(fun k -> Loc.span_compare k loc = 0)
          in
          (SpanMap.add orig_loc (locs, Specific new_codes) suppressions, used_universal_suppressions)
      | Some (_, All l) ->
        let universal =
          CodeSet.fold (fun (code, _) -> CodeLocSet.add (code, l)) codes CodeLocSet.empty
        in
        let old_universal =
          SpanMap.find_opt loc used_universal_suppressions
          |> Base.Option.value ~default:CodeLocSet.empty
        in
        let universal = CodeLocSet.union universal old_universal in
        let orig_loc =
          (* We don't want to overwrite the original location with one that falls inside it *)
          SpanMap.keys suppressions |> Base.List.find_exn ~f:(fun k -> Loc.span_compare k loc = 0)
        in
        let used_universal_suppressions =
          SpanMap.add orig_loc universal used_universal_suppressions
        in
        (* Using a univeral suppression to suppress a single code should remove it from the map, but
           we need to mark the codes it suppressed in the past *)
        (SpanMap.remove loc suppressions, used_universal_suppressions)
      | None -> begin
        match SpanMap.find_opt loc used_universal_suppressions with
        | None -> (SpanMap.remove loc suppressions, used_universal_suppressions)
        | Some old_universal ->
          let supp_loc = CodeLocSet.choose old_universal |> snd in
          let universal =
            CodeSet.fold (fun (code, _) -> CodeLocSet.add (code, supp_loc)) codes old_universal
          in
          let used_universal_suppressions =
            SpanMap.add supp_loc universal used_universal_suppressions
          in
          (SpanMap.remove loc suppressions, used_universal_suppressions)
      end
      | _ -> (suppressions, used_universal_suppressions)
    in
    { suppressions; lint_suppressions; used_universal_suppressions }

  let union a b =
    {
      suppressions = SpanMap.union a.suppressions b.suppressions;
      lint_suppressions = LocSet.union a.lint_suppressions b.lint_suppressions;
      used_universal_suppressions =
        SpanMap.union a.used_universal_suppressions b.used_universal_suppressions;
    }

  let add_lint_suppression lint_suppression t =
    { t with lint_suppressions = LocSet.add lint_suppression t.lint_suppressions }

  let remove_lint_suppression lint_suppression ({ lint_suppressions; _ } as orig) =
    { orig with lint_suppressions = LocSet.remove lint_suppression lint_suppressions }

  let suppression_at_loc loc { suppressions; _ } = SpanMap.find_opt loc suppressions
end

type t = FileSuppressions.t FilenameMap.t

let empty = FilenameMap.empty

let add loc codes map =
  let file = file_of_loc_unsafe loc in
  let suppressions = FileSuppressions.empty |> FileSuppressions.add loc codes in
  FilenameMap.add ~combine:FileSuppressions.union file suppressions map

let union =
  let combine _key x y = Some (FileSuppressions.union x y) in
  (fun a b -> Utils_js.FilenameMap.union ~combine a b)

let add_lint_suppressions lint_suppressions map =
  LocSet.fold
    begin
      fun loc acc ->
        let file = file_of_loc_unsafe loc in
        let file_suppressions =
          FilenameMap.find_opt file acc |> Base.Option.value ~default:FileSuppressions.empty
        in
        let file_suppressions = FileSuppressions.add_lint_suppression loc file_suppressions in
        FilenameMap.add file file_suppressions acc
    end
    lint_suppressions
    map

let remove = FilenameMap.remove

(* raises if `loc` has no filename *)
let file_suppressions_of_loc loc suppressions_map =
  let file = file_of_loc_unsafe loc in
  match FilenameMap.find_opt file suppressions_map with
  | Some x -> x
  | None -> FileSuppressions.empty

(* raises if `loc` has no filename *)
let suppression_at_loc loc suppressions_map =
  let file_suppressions = file_suppressions_of_loc loc suppressions_map in
  FileSuppressions.suppression_at_loc loc file_suppressions

(* raises if `loc` has no filename.
 * no-op if suppressions_map does not contain an entry for that file. *)
let update_file_suppressions f loc suppressions_map =
  let file = file_of_loc_unsafe loc in
  match FilenameMap.find_opt file suppressions_map with
  | None -> suppressions_map
  | Some file_suppressions ->
    let file_suppressions = f file_suppressions in
    FilenameMap.add file file_suppressions suppressions_map

let remove_suppression_from_map loc specific_codes (suppressions_map : t) =
  update_file_suppressions (FileSuppressions.remove loc specific_codes) loc suppressions_map

let remove_lint_suppression_from_map loc (suppressions_map : t) =
  update_file_suppressions (FileSuppressions.remove_lint_suppression loc) loc suppressions_map

type error_code_kind =
  | ErrorCodeUnsuppressable
  | ErrorCodeRequireSpecific
  | ErrorCodeGeneral

let check_loc ~error_code_migration suppressions specific_codes (result, (unused : t)) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  let specific_codes_set = lazy (specific_codes |> CodeMap.keys |> CodeSet.of_list) in
  let suppression_applies codes2 =
    match codes2 with
    | All _ ->
      CodeMap.for_all
        (fun _code -> function
          | ErrorCodeUnsuppressable
          | ErrorCodeRequireSpecific ->
            false
          | ErrorCodeGeneral -> true)
        specific_codes
    | Specific specific_codes2 ->
      CodeMap.for_all
        (fun code -> function
          | ErrorCodeUnsuppressable -> false
          | ErrorCodeRequireSpecific
          | ErrorCodeGeneral ->
            (match (error_code_migration, code) with
            | ( Options.ErrorCodeMigration.Compatibility,
                ( ( "incompatible-call" | "incompatible-cast" | "incompatible-extend"
                  | "incompatible-return" | "incompatible-type-arg" | "prop-missing" ),
                  code_loc
                )
              ) ->
              CodeSet.mem code specific_codes2
              || CodeSet.mem ("incompatible-type", code_loc) specific_codes2
            | _ -> CodeSet.mem code specific_codes2))
        specific_codes
  in
  match suppression_at_loc loc suppressions with
  | Some (locs, codes') when suppression_applies codes' ->
    let used = locs in
    let unused = remove_suppression_from_map loc (Lazy.force specific_codes_set) unused in
    let unused =
      match (error_code_migration, codes') with
      | (Options.ErrorCodeMigration.Compatibility, Specific _) ->
        let set = Lazy.force specific_codes_set in
        if
          CodeSet.mem ("incompatible-call", loc) set
          || CodeSet.mem ("incompatible-cast", loc) set
          || CodeSet.mem ("incompatible-extend", loc) set
          || CodeSet.mem ("incompatible-return", loc) set
          || CodeSet.mem ("incompatible-type-arg", loc) set
          || CodeSet.mem ("prop-missing", loc) set
        then
          remove_suppression_from_map loc (CodeSet.singleton ("incompatible-type", loc)) unused
        else
          unused
      | _ -> unused
    in
    (Off, used, unused)
  | _ -> (result, LocSet.empty, unused)

let in_node_modules ~root ~file_options loc =
  match Base.Option.both (Loc.source loc) file_options with
  | None -> false
  | Some (file, options) -> Files.is_within_node_modules ~root ~options (File_key.to_string file)

let in_declarations ~file_options loc =
  match Base.Option.both (Loc.source loc) file_options with
  | None -> false
  | Some (file, options) -> Files.is_declaration options (File_key.to_string file)

let check
    ~root
    ~file_options
    ~error_code_migration
    ~unsuppressable_error_codes
    (err : 'loc Flow_intermediate_error_types.intermediate_error)
    (suppressions : t)
    (unused : t) =
  let loc = err.Flow_intermediate_error_types.loc in
  let code_opt =
    Base.Option.map err.Flow_intermediate_error_types.error_code ~f:(fun code ->
        let string_code = Error_codes.string_of_code code in
        let kind =
          if SSet.mem string_code unsuppressable_error_codes then
            ErrorCodeUnsuppressable
          else if Error_codes.require_specific code then
            ErrorCodeRequireSpecific
          else
            ErrorCodeGeneral
        in
        CodeMap.singleton (string_code, loc) kind
    )
  in
  (* Ignore lint errors from node modules, and all errors from declarations directories. *)
  let ignore =
    match err.Flow_intermediate_error_types.kind with
    | Flow_errors_utils.LintError _ ->
      in_node_modules ~root ~file_options loc || in_declarations ~file_options loc
    | _ -> in_declarations ~file_options loc
  in
  match (ignore, code_opt) with
  | (true, _) -> None
  | (_, None) -> Some (Err, LocSet.empty, unused)
  | (_, Some specific_codes) ->
    let (result, used, unused) =
      check_loc suppressions ~error_code_migration specific_codes (Err, unused) loc
    in
    let result =
      match err.Flow_intermediate_error_types.kind with
      | Flow_errors_utils.RecursionLimitError ->
        (* TODO: any related suppressions should not be considered used *)
        Err
      | _ -> result
    in
    Some (result, used, unused)

(** Gets the locations of the suppression comments that are yet unused *)
let all_unused_locs map =
  FilenameMap.fold
    (fun _k v acc -> LocSet.union acc (FileSuppressions.all_unused_locs v))
    map
    LocSet.empty

let universally_suppressed_codes map =
  FilenameMap.fold
    (fun _k v acc -> CodeLocSet.union acc (FileSuppressions.universally_suppressed_codes v))
    map
    CodeLocSet.empty

let filter_suppressed_errors
    ~root
    ~file_options
    ~error_code_migration
    ~unsuppressable_error_codes
    ~loc_of_aloc
    suppressions
    errors
    ~unused =
  (* Filter out suppressed errors. also track which suppressions are used. *)
  Flow_error.ErrorSet.fold
    (fun error ((errors, suppressed, unused) as acc) ->
      let error =
        Flow_intermediate_error.make_intermediate_error
          ~loc_of_aloc
          ~updated_error_code:(error_code_migration = Options.ErrorCodeMigration.New)
          error
      in
      match
        check
          ~root
          ~file_options
          ~error_code_migration
          ~unsuppressable_error_codes
          error
          suppressions
          unused
      with
      | None -> acc
      | Some (severity, used, unused) ->
        (match severity with
        | Off -> (errors, (error, used) :: suppressed, unused)
        | _ ->
          let error =
            let open Flow_intermediate_error_types in
            match error.error_code with
            | Some code when SSet.mem (Error_codes.string_of_code code) unsuppressable_error_codes
              ->
              { error with unsuppressable = true }
            | _ -> error
          in
          let error =
            Flow_intermediate_error.to_printable_error ~loc_of_aloc ~strip_root:(Some root) error
          in
          (Flow_errors_utils.ConcreteLocPrintableErrorSet.add error errors, suppressed, unused)))
    errors
    (Flow_errors_utils.ConcreteLocPrintableErrorSet.empty, [], unused)

let find_error_suppressions_to_change_error_code_for_codemod
    ~root ~file_options ~unsuppressable_error_codes ~loc_of_aloc ~suppressions ~errors =
  (* Filter out suppressed errors. also track which suppressions are used. *)
  Flow_error.ErrorSet.fold
    (fun error ((suppressed, unused) as acc) ->
      let error_old =
        Flow_intermediate_error.make_intermediate_error ~loc_of_aloc ~updated_error_code:false error
      in
      let error_new =
        Flow_intermediate_error.make_intermediate_error ~loc_of_aloc ~updated_error_code:true error
      in
      let error_code_old = error_old.Flow_intermediate_error_types.error_code in
      let error_code_new = error_new.Flow_intermediate_error_types.error_code in
      (* The error code needs to be migrated if old code != new code (incompatible-type) *)
      if error_code_old <> error_code_new && error_code_new = Some Error_codes.IncompatibleType then
        (* Use the same error suppression checking logic to find the location that needs to be changed. *)
        match
          check
            ~root
            ~file_options
            ~error_code_migration:Options.ErrorCodeMigration.Old
            ~unsuppressable_error_codes
            error_old
            suppressions
            unused
        with
        | None -> acc
        | Some (severity, used, unused) ->
          (match severity with
          | Off -> ((error, used) :: suppressed, unused)
          | _ -> acc)
      else
        acc)
    errors
    ([], suppressions)
  |> fst
  |> Base.List.fold ~init:LocSet.empty ~f:(fun acc (_, locs) -> LocSet.union acc locs)

let update_suppressions current_suppressions new_suppressions =
  FilenameMap.fold
    begin
      fun file file_suppressions acc ->
        if FileSuppressions.is_empty file_suppressions then
          FilenameMap.remove file acc
        else
          FilenameMap.add file file_suppressions acc
    end
    new_suppressions
    current_suppressions

let get_lint_settings severity_cover loc =
  Base.Option.Monad_infix.(
    Loc.source loc >>= fun source ->
    Utils_js.FilenameMap.find_opt source severity_cover >>= ExactCover.find_opt loc
  )

(* Filter out lint errors which are definitely suppressed or were never
 * enabled in the first place. *)
let filter_lints suppressions errors aloc_tables ~include_suppressions severity_cover =
  Flow_error.(
    ErrorSet.fold
      (fun error (errors, warnings, suppressions) ->
        Severity.(
          match (msg_of_error error |> Error_message.kind_of_msg, loc_of_error error) with
          | (Flow_errors_utils.LintError lint_kind, Some loc) ->
            let loc = ALoc.to_loc_with_tables aloc_tables loc in
            begin
              match get_lint_settings severity_cover loc with
              | None ->
                (* This shouldn't happen -- the primary location of a lint error
                 * should always be in the file where the error was found. Until we
                 * are more confident that this invariant holds, pass the lint warning
                 * back to the master process, where it will be filtered in the
                 * context of the full severity cover set. *)
                (errors, ErrorSet.add error warnings, suppressions)
              | Some lint_settings ->
                (* Lint settings can only affect lint errors when located at the
                 * error's "primary" location. This is a nice property, since it means
                 * we can filter out some lint errors here instead of passing them
                 * back and filtering them later.
                 *
                 * Note that a lint error might still be filtered out later, since a
                 * lint error can be suppressed by a "regular" suppression comment. *)
                (match LintSettings.get_value lint_kind lint_settings with
                | Off when include_suppressions ->
                  (* When --include-suppressions is active we only want to remove lints that were
                     never enabled in the first place, as opposed to those that are enabled but
                     suppressed. We also add them as an error regardless of what they were in the
                     first place. *)
                  if LintSettings.is_explicit lint_kind lint_settings then
                    (ErrorSet.add error errors, warnings, suppressions)
                  else
                    (errors, warnings, suppressions)
                | Off ->
                  let suppressions =
                    match LintSettings.get_loc lint_kind lint_settings with
                    | Some used_suppression ->
                      remove_lint_suppression_from_map used_suppression suppressions
                    | _ -> suppressions
                  in
                  (errors, warnings, suppressions)
                | Warn -> (errors, ErrorSet.add error warnings, suppressions)
                | Err -> (ErrorSet.add error errors, warnings, suppressions))
            end
          (* Non-lint errors can be suppressed by any location present in the error.
           * A dependency location might be part of the error, and the corresponding
           * suppression is not available from this worker. We need to pass back all
           * errors to be filtered in the master process. *)
          | _ -> (ErrorSet.add error errors, warnings, suppressions)
        ))
      errors
      (ErrorSet.empty, ErrorSet.empty, suppressions)
  )

let filter_by_file files m = FilenameMap.filter (fun file _ -> FilenameSet.mem file files) m
