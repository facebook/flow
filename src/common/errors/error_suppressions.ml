(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is a data structure used to track what locations are being suppressed
 * and which suppressions have yet to be used.
 *)

open Severity
open Utils_js

type error_suppressions = LocSet.t SpanMap.t
type t = {
  suppressions: error_suppressions;
  unused: error_suppressions;
  unused_lint_suppressions: LocSet.t
}

let empty = {
  suppressions = SpanMap.empty;
  unused = SpanMap.empty;
  unused_lint_suppressions = LocSet.empty;
}

let add loc { suppressions; unused; unused_lint_suppressions; } = Loc.(
  let start = { loc.start with line = loc._end.line + 1; column = 0 } in
  let _end = { loc._end with line = loc._end.line + 2; column = 0 } in
  let suppression_loc = { loc with start; _end; } in
  let combine = (LocSet.union) in
  let set = LocSet.singleton loc in
  {
    suppressions = SpanMap.add ~combine suppression_loc set suppressions;
    unused = SpanMap.add ~combine suppression_loc set unused;
    unused_lint_suppressions;
  }
)

let union a b = {
  suppressions = SpanMap.union a.suppressions b.suppressions;
  unused = SpanMap.union a.unused b.unused;
  unused_lint_suppressions = LocSet.union a.unused_lint_suppressions b.unused_lint_suppressions;
}

let add_unused_lint_suppressions unused_lint_suppressions t = {
  t with
  unused_lint_suppressions = LocSet.union t.unused_lint_suppressions unused_lint_suppressions;
}

let check_loc ((result, consumed,
      { suppressions; unused; unused_lint_suppressions; },
      lint_kind, severity_cover) as acc) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  if SpanMap.mem loc suppressions
  then
    let locs = SpanMap.find_unsafe loc suppressions in
    let consumed = LocSet.union locs consumed in
    Off, consumed, { suppressions; unused = SpanMap.remove loc unused;
      unused_lint_suppressions}, lint_kind, severity_cover
  else
    Option.value_map lint_kind ~default:acc
    ~f:(fun some_lint_kind ->
      let state = ExactCover.get_severity some_lint_kind loc severity_cover in
      let unused_lint_suppressions = if state = Off then
        let settings_at_loc = ExactCover.find loc severity_cover in
        let used_lint_suppression = LintSettings.get_loc some_lint_kind settings_at_loc in
        Option.value_map used_lint_suppression
          ~f:(fun used_suppression -> LocSet.remove used_suppression unused_lint_suppressions)
          ~default:unused_lint_suppressions
      else unused_lint_suppressions in
      severity_min state result, consumed, { suppressions; unused;
        unused_lint_suppressions; }, lint_kind, severity_cover)

(* Checks if any of the given locations should be suppressed. *)
let check_locs (locs: Loc.t list) suppressions lint_kind severity_cover =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression *)
  let (suppression_state, consumed, suppressions, _, _) =
    List.fold_left check_loc (Err, LocSet.empty, suppressions,
      lint_kind, severity_cover) locs
  in (suppression_state, consumed, suppressions)

let check err severity_cover suppressions =
  let locs = Errors.locs_of_error err in
  let lint_kind =
    let open Errors in
    match kind_of_error err with
      | LintError lint_kind -> Some lint_kind
      | _ -> None
  in
  let (result, consumed, { suppressions; unused; unused_lint_suppressions; }) =
    check_locs locs suppressions lint_kind severity_cover
  in
  (* Ignore lints in node_modules folders (which we assume to be dependencies). *)
  let is_in_dependency =
    let primary_loc = Errors.loc_of_error err in
    Option.value_map (Loc.source primary_loc) ~default:false ~f:(fun filename ->
      String_utils.is_substring "/node_modules/" (File_key.to_string filename))
  in
  let result = match Errors.kind_of_error err with
    | Errors.RecursionLimitError -> Err
    | _ -> if (is_in_dependency && (Option.is_some lint_kind))
      then Off
      else result
  in (result, consumed, { suppressions; unused; unused_lint_suppressions; })

(* Gets the locations of the suppression comments that are yet unused *)
let unused { unused; unused_lint_suppressions; _; } =
  unused
  |> SpanMap.values
  |> List.fold_left LocSet.union unused_lint_suppressions
  |> LocSet.elements

let is_empty { suppressions; unused; unused_lint_suppressions; } =
  SpanMap.is_empty suppressions && SpanMap.is_empty unused
    && LocSet.is_empty unused_lint_suppressions

let union_suppressions suppressions =
  (* union suppressions from all files together *)
  FilenameMap.fold
    (fun _key -> union)
    suppressions
    empty

let filter_suppressed_errors =
  let is_explicit error severity_cover =
    match Errors.kind_of_error error with
      | Errors.LintError kind ->
        let locs = Errors.locs_of_error error in
        List.fold_left
          (fun acc loc -> acc || ExactCover.is_explicit kind loc severity_cover)
          false locs
      | _ -> true
  in

  fun suppressions severity_cover errors ->
    (* Filter out suppressed errors. also track which suppressions are used. *)
    let errors, warnings, suppressed_errors, suppressions = Errors.ErrorSet.fold
      (fun error (errors, warnings, suppressed_errors, supp_acc) ->
        let (suppression_state, consumed_suppressions, supp_acc) =
          check error severity_cover supp_acc
        in
        let errors, warnings, suppressed_errors = match suppression_state with
          | Off ->
            (* Don't treat a flow lint as suppressed if it was never enabled in the first place. *)
            if is_explicit error severity_cover
            then errors, warnings, (error, consumed_suppressions)::suppressed_errors
            else errors, warnings, suppressed_errors
          | Warn -> errors, Errors.ErrorSet.add error warnings, suppressed_errors
          | Err -> Errors.ErrorSet.add error errors, warnings, suppressed_errors
        in
        errors, warnings, suppressed_errors, supp_acc
      ) errors (Errors.ErrorSet.empty, Errors.ErrorSet.empty, [], suppressions)
    in
    (* these are bound just so it's more obvious what is being returned *)
    (errors, warnings, suppressed_errors, suppressions)
