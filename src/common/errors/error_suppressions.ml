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
  lint_suppressions: LocSet.t
}

let empty = {
  suppressions = SpanMap.empty;
  lint_suppressions = LocSet.empty;
}

let add loc { suppressions; lint_suppressions } =
  let suppression_loc = Loc.(
    let start = { loc.start with line = loc._end.line + 1; column = 0 } in
    let _end = { loc._end with line = loc._end.line + 2; column = 0 } in
    { loc with start; _end }
  ) in
  let suppressions =
    SpanMap.add suppression_loc (LocSet.singleton loc) suppressions
      ~combine:LocSet.union
  in
  { suppressions; lint_suppressions }

let union a b = {
  suppressions = SpanMap.union a.suppressions b.suppressions;
  lint_suppressions = LocSet.union a.lint_suppressions b.lint_suppressions;
}

let add_lint_suppressions lint_suppressions t = {
  t with
  lint_suppressions = LocSet.union t.lint_suppressions lint_suppressions;
}

let check_loc lint_kind {suppressions; _} severity_cover
  ((result, used, unused, is_primary_loc) as acc) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  if SpanMap.mem loc suppressions
  then
    let locs = SpanMap.find_unsafe loc suppressions in
    let used = LocSet.union locs used in
    let unused = {
      unused with
      suppressions = SpanMap.remove loc unused.suppressions
    } in
    Off, used, unused, false
  else
    Option.value_map lint_kind ~default:acc ~f:(fun some_lint_kind ->
      let lint_settings = ExactCover.find loc severity_cover in
      let state = LintSettings.get_value some_lint_kind lint_settings in
      let unused_lint_suppressions =
        match LintSettings.get_loc some_lint_kind lint_settings with
        | Some used_suppression when state = Off ->
          (* TODO: consume this lint suppression by adding to used set *)
          LocSet.remove used_suppression unused.lint_suppressions
        | _ -> unused.lint_suppressions
      in
      let unused = {unused with lint_suppressions = unused_lint_suppressions} in
      if (is_primary_loc && state == Off) || LintSettings.is_explicit some_lint_kind lint_settings
      then severity_min state result, used, unused, false
      else result, used, unused, false
    )

(* Checks if any of the given locations should be suppressed. *)
let check_locs locs lint_kind suppressions severity_cover unused =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression.
     If the "primary" location has severity = Off, the error should be
     suppressed even if it is not explicit. *)
  List.fold_left
    (check_loc lint_kind suppressions severity_cover)
    (Err, LocSet.empty, unused, true)
    locs

let check err suppressions severity_cover unused =
  let locs = Errors.locs_of_error err in
  (* Ignore lint errors which were never enabled in the first place. *)
  let lint_kind, ignore =
    match Errors.kind_of_error err with
      | Errors.LintError kind ->
        let severity, is_explicit = List.fold_left (fun (s, e) loc ->
          let lint_settings = ExactCover.find loc severity_cover in
          let s' = LintSettings.get_value kind lint_settings in
          let e' = LintSettings.is_explicit kind lint_settings in
          (severity_min s s', e || e')
        ) (Err, false) locs in
        let ignore = severity = Off && not is_explicit in
        Some kind, ignore
      | _ -> None, false
  in
  if ignore then None else
  let result, used, unused, _ =
    check_locs locs lint_kind suppressions severity_cover unused
  in
  (* Ignore lints in node_modules folders (which we assume to be dependencies). *)
  let is_in_dependency =
    let primary_loc = Errors.loc_of_error err in
    Option.value_map (Loc.source primary_loc) ~default:false ~f:(fun filename ->
      String_utils.is_substring "/node_modules/" (File_key.to_string filename))
  in
  let result = match Errors.kind_of_error err with
    | Errors.RecursionLimitError ->
      (* TODO: any related suppressions should not be considered used *)
      Err
    | _ -> if (is_in_dependency && (Option.is_some lint_kind))
      then Off (* TODO: this should not show up with --include-suppressed *)
      else result
  in Some (result, used, unused)

(* Gets the locations of the suppression comments that are yet unused *)
let unused { suppressions; lint_suppressions } =
  suppressions
  |> SpanMap.values
  |> List.fold_left LocSet.union lint_suppressions
  |> LocSet.elements

let is_empty { suppressions; lint_suppressions } =
  SpanMap.is_empty suppressions && LocSet.is_empty lint_suppressions

let union_suppressions suppressions =
  (* union suppressions from all files together *)
  FilenameMap.fold
    (fun _key -> union)
    suppressions
    empty

let filter_suppressed_errors suppressions severity_cover errors ~unused =
  (* Filter out suppressed errors. also track which suppressions are used. *)
  Errors.ErrorSet.fold (fun error ((errors, warnings, suppressed, unused) as acc) ->
    match check error suppressions severity_cover unused with
    | None -> acc
    | Some (severity, used, unused) ->
      match severity with
      | Off -> errors, warnings, (error, used)::suppressed, unused
      | Warn -> errors, Errors.ErrorSet.add error warnings, suppressed, unused
      | Err -> Errors.ErrorSet.add error errors, warnings, suppressed, unused
  ) errors (Errors.ErrorSet.empty, Errors.ErrorSet.empty, [], unused)
