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
}

let empty = {
  suppressions = SpanMap.empty;
  unused = SpanMap.empty;
}

let add loc { suppressions; unused; } = Loc.(
  let start = { loc.start with line = loc._end.line + 1; column = 0 } in
  let _end = { loc._end with line = loc._end.line + 2; column = 0 } in
  let suppression_loc = { loc with start; _end; } in
  let combine = (Loc.LocSet.union) in
  let set = Loc.LocSet.singleton loc in
  {
    suppressions = SpanMap.add ~combine suppression_loc set suppressions;
    unused = SpanMap.add ~combine suppression_loc set unused;
  }
)

let union a b = {
  suppressions = SpanMap.union a.suppressions b.suppressions;
  unused = SpanMap.union a.unused b.unused;
}

let check_loc ((_result, consumed, { suppressions; unused; }) as acc) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.({ loc with _end = loc.start; }) in
  if SpanMap.mem loc suppressions
  then
    let locs = SpanMap.find_unsafe loc suppressions in
    let consumed = Loc.LocSet.union locs consumed in
    true, consumed, { suppressions; unused = SpanMap.remove loc unused}
  else acc

(* Checks if any of the given locations should be suppressed. *)
let check_locs (locs: Loc.t list) suppressions =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression *)
  List.fold_left check_loc (false, Loc.LocSet.empty, suppressions) locs

let check err lint_settings suppressions =
  let locs = Errors.locs_of_error err in
  let (suppressed, consumed, suppressions) as result = check_locs locs suppressions in
  if suppressed then result
  else
    let err_kind = Errors.kind_of_error err in
    match err_kind with
    | Errors.LintError lint_kind ->
      (* TODO: (rballard) use the location information when that gets added in. *)
      (* TODO: (rballard) track unused lint suppressions. *)
      let suppressed = LintSettings.is_suppressed lint_kind lint_settings in
      (suppressed, consumed, suppressions)
    | _ -> result

(* Get's the locations of the suppression comments that are yet unused *)
let unused { unused; _; } =
  unused
  |> SpanMap.values
  |> List.fold_left Loc.LocSet.union Loc.LocSet.empty
  |> Loc.LocSet.elements

let cardinal { suppressions; unused } =
  SpanMap.cardinal suppressions + SpanMap.cardinal unused

let is_empty { suppressions; unused; } =
  SpanMap.is_empty suppressions && SpanMap.is_empty unused

let union_suppressions suppressions =
  (* union suppressions from all files together *)
  FilenameMap.fold
    (fun _key -> union)
    suppressions
    empty

let filter_suppressed_errors suppressions lint_settings errors =
  (* Filter out suppressed errors. also track which suppressions are used. *)
  let errors, suppressed_errors, suppressions = Errors.ErrorSet.fold
    (fun error (errors, suppressed_errors, supp_acc) ->
      let (suppressed, consumed_suppressions, supp_acc) =
        check error lint_settings supp_acc
      in
      let errors, suppressed_errors = if suppressed
        then errors, (error, consumed_suppressions)::suppressed_errors
        else Errors.ErrorSet.add error errors, suppressed_errors in
      errors, suppressed_errors, supp_acc
    ) errors (Errors.ErrorSet.empty, [], suppressions)
  in
  (* these are bound just so it's more obvious what is being returned *)
  (errors, suppressed_errors, suppressions)
