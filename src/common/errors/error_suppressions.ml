(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This is a data structure used to track what locations are being suppressed
 * and which suppressions have yet to be used.
 *)

open Span
open Utils_js

type error_suppressions = Loc.LocSet.t SpanMap.t
type t = {
  suppressions: error_suppressions;
  unused: error_suppressions;
  unused_lint_suppressions: Loc.LocSet.t
}

let empty = {
  suppressions = SpanMap.empty;
  unused = SpanMap.empty;
  unused_lint_suppressions = Loc.LocSet.empty;
}

let add loc { suppressions; unused; unused_lint_suppressions; } = Loc.(
  let start = { loc.start with line = loc._end.line + 1; column = 0 } in
  let _end = { loc._end with line = loc._end.line + 2; column = 0 } in
  let suppression_loc = { loc with start; _end; } in
  let combine = (Loc.LocSet.union) in
  let set = Loc.LocSet.singleton loc in
  {
    suppressions = SpanMap.add ~combine suppression_loc set suppressions;
    unused = SpanMap.add ~combine suppression_loc set unused;
    unused_lint_suppressions;
  }
)

let union a b = {
  suppressions = SpanMap.union a.suppressions b.suppressions;
  unused = SpanMap.union a.unused b.unused;
  unused_lint_suppressions = Loc.LocSet.union a.unused_lint_suppressions b.unused_lint_suppressions;
}

let set_unused_lint_suppressions unused_lint_suppressions t = {t with unused_lint_suppressions}

let check_loc ((result, consumed,
      { suppressions; unused; unused_lint_suppressions; },
      lint_kind, lint_settings) as acc) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  if SpanMap.mem loc suppressions
  then
    let locs = SpanMap.find_unsafe loc suppressions in
    let consumed = Loc.LocSet.union locs consumed in
    LintSettings.Off, consumed, { suppressions; unused = SpanMap.remove loc unused;
      unused_lint_suppressions}, lint_kind, lint_settings
  else
    Option.value_map lint_kind ~default:acc
    ~f:(fun some_lint_kind ->
      let state = LintSettingsMap.get_state some_lint_kind loc lint_settings in
      let unused_lint_suppressions = if state = LintSettings.Off then
        let settings_at_loc = LintSettingsMap.settings_at_loc loc lint_settings in
        let used_lint_suppression = LintSettings.get_loc some_lint_kind settings_at_loc in
        Option.value_map used_lint_suppression
          ~f:(fun used_suppression -> Loc.LocSet.remove used_suppression unused_lint_suppressions)
          ~default:unused_lint_suppressions
      else unused_lint_suppressions in
      LintSettings.state_min state result, consumed, { suppressions; unused;
        unused_lint_suppressions; }, lint_kind, lint_settings)

(* Checks if any of the given locations should be suppressed. *)
let check_locs (locs: Loc.t list) suppressions lint_kind lint_settings =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression *)
  let (suppression_state, consumed, suppressions, _, _) =
    List.fold_left check_loc (LintSettings.Err, Loc.LocSet.empty, suppressions,
      lint_kind, lint_settings) locs
  in (suppression_state, consumed, suppressions)

let check err lint_settings suppressions =
  let locs = Errors.locs_of_error err in
  let lint_kind =
    let open Errors in
    match kind_of_error err with
      | LintError lint_kind -> Some lint_kind
      | _ -> None
  in
  let (result, consumed, { suppressions; unused; unused_lint_suppressions; }) =
    check_locs locs suppressions lint_kind lint_settings
  in
  (* Ignore lints in node_modules folders (which we assume to be dependencies). *)
  let is_in_dependency = match Errors.infos_of_error err with
    | (primary_loc,_)::_ ->
      Option.value_map (Loc.source primary_loc) ~default:false ~f:(fun filename ->
        String_utils.is_substring "/node_modules/" (Loc.string_of_filename filename))
    | [] -> false
  in
  let result = if is_in_dependency && (Option.is_some lint_kind)
    then LintSettings.Off
    else result
  in (result, consumed, { suppressions; unused; unused_lint_suppressions; })

(* Gets the locations of the suppression comments that are yet unused *)
let unused { unused; unused_lint_suppressions; _; } =
  unused
  |> SpanMap.values
  |> List.fold_left Loc.LocSet.union unused_lint_suppressions
  |> Loc.LocSet.elements

let is_empty { suppressions; unused; unused_lint_suppressions; } =
  SpanMap.is_empty suppressions && SpanMap.is_empty unused
    && Loc.LocSet.is_empty unused_lint_suppressions

let union_suppressions suppressions =
  (* union suppressions from all files together *)
  FilenameMap.fold
    (fun _key -> union)
    suppressions
    empty

let filter_suppressed_errors =
  let is_explicit error lint_settings =
    match Errors.kind_of_error error with
      | Errors.LintError kind ->
        let locs = Errors.locs_of_error error in
        List.fold_left
          (fun acc loc -> acc || LintSettingsMap.is_explicit kind loc lint_settings)
          false locs
      | _ -> true
  in

  fun suppressions lint_settings errors ->
    (* Filter out suppressed errors. also track which suppressions are used. *)
    let errors, warnings, suppressed_errors, suppressions = Errors.ErrorSet.fold
      (fun error (errors, warnings, suppressed_errors, supp_acc) ->
        let (suppression_state, consumed_suppressions, supp_acc) =
          check error lint_settings supp_acc
        in
        let errors, warnings, suppressed_errors = match suppression_state with
          | LintSettings.Off ->
            (* Don't treat a flow lint as suppressed if it was never enabled in the first place. *)
            if is_explicit error lint_settings
            then errors, warnings, (error, consumed_suppressions)::suppressed_errors
            else errors, warnings, suppressed_errors
          | LintSettings.Warn -> errors, Errors.ErrorSet.add error warnings, suppressed_errors
          | LintSettings.Err -> Errors.ErrorSet.add error errors, warnings, suppressed_errors
        in
        errors, warnings, suppressed_errors, supp_acc
      ) errors (Errors.ErrorSet.empty, Errors.ErrorSet.empty, [], suppressions)
    in
    (* these are bound just so it's more obvious what is being returned *)
    (errors, warnings, suppressed_errors, suppressions)
