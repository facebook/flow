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

type t_map = t FilenameMap.t

exception No_source of string
exception Missing_lint_settings of string

let empty = {
  suppressions = SpanMap.empty;
  lint_suppressions = LocSet.empty;
}

let file_of_loc_unsafe loc =
  match loc.Loc.source with
  | Some x -> x
  | None -> raise (No_source (Loc.to_string loc))

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

let add_to_map loc map =
  let file = file_of_loc_unsafe loc in
  let suppressions = FilenameMap.get file map |> Option.value ~default:empty in
  let suppressions = add loc suppressions in
  FilenameMap.add file suppressions map

let union a b = {
  suppressions = SpanMap.union a.suppressions b.suppressions;
  lint_suppressions = LocSet.union a.lint_suppressions b.lint_suppressions;
}

let union_maps =
  let combine _key x y = Some (union x y) in
  fun a b -> Utils_js.FilenameMap.union ~combine a b

let add_lint_suppression lint_suppression t = {
  t with
  lint_suppressions = LocSet.add lint_suppression t.lint_suppressions;
}

let add_lint_suppressions_to_map lint_suppressions map =
  LocSet.fold begin fun loc acc ->
    let file = file_of_loc_unsafe loc in
    let file_suppressions = FilenameMap.get file acc |> Option.value ~default:empty in
    let file_suppressions = add_lint_suppression loc file_suppressions in
    FilenameMap.add file file_suppressions acc
  end lint_suppressions map

(* raises if `loc` has no filename or `severity_cover` contains no entry for `loc`'s filename *)
let lint_settings_at_loc loc severity_cover =
  let file = file_of_loc_unsafe loc in
  let file_cover = match FilenameMap.get file severity_cover with
  | Some x -> x
  | None -> raise (Missing_lint_settings (Loc.to_string loc))
  in
  ExactCover.find loc file_cover

(* raises if `loc` has no filename *)
let file_suppressions_of_loc loc suppressions_map =
  let file = file_of_loc_unsafe loc in
  match FilenameMap.get file suppressions_map with
  | Some x -> x
  | None -> empty

(* raises if `loc` has no filename *)
let suppression_at_loc loc suppressions_map =
  let file_suppressions = file_suppressions_of_loc loc suppressions_map in
  let {suppressions; _} = file_suppressions in
  SpanMap.get loc suppressions

(* raises if `loc` has no filename.
 * no-op if suppressions_map does not contain an entry for that file. *)
let update_file_suppressions f loc suppressions_map =
  let file = file_of_loc_unsafe loc in
  match FilenameMap.get file suppressions_map with
  | None -> suppressions_map
  | Some file_suppressions ->
    let file_suppressions = f file_suppressions in
    FilenameMap.add file file_suppressions suppressions_map

let remove_suppression_from_map loc (suppressions_map: t_map) =
  let f file_suppressions =
    let suppressions = SpanMap.remove loc file_suppressions.suppressions in
    { file_suppressions with suppressions }
  in
  update_file_suppressions f loc suppressions_map

let remove_lint_suppression_from_map loc (suppressions_map: t_map) =
  let f file_suppressions =
    let lint_suppressions = LocSet.remove loc file_suppressions.lint_suppressions in
    { file_suppressions with lint_suppressions }
  in
  update_file_suppressions f loc suppressions_map

let check_loc lint_kind suppressions severity_cover
  ((result, used, (unused: t_map), is_primary_loc) as acc) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  match suppression_at_loc loc suppressions with
  | Some locs ->
    let used = LocSet.union locs used in
    let unused = remove_suppression_from_map loc unused in
    Off, used, unused, false
  | None ->
    (* Only respect lint settings at the primary (first) location *)
    if is_primary_loc
    then Option.value_map lint_kind ~default:acc ~f:(fun some_lint_kind ->
      let lint_settings = lint_settings_at_loc loc severity_cover in
      let state = LintSettings.get_value some_lint_kind lint_settings in
      let unused =
        match LintSettings.get_loc some_lint_kind lint_settings with
        | Some used_suppression when state = Off ->
          (* TODO: consume this lint suppression by adding to used set *)
          remove_lint_suppression_from_map used_suppression unused
        | _ -> unused
      in
      state, used, unused, false
    )
    else result, used, unused, false

(* Checks if any of the given locations should be suppressed. *)
let check_locs locs lint_kind (suppressions: t_map) severity_cover (unused: t_map) =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression.
     If the "primary" location has severity = Off, the error should be
     suppressed even if it is not explicit. *)
  List.fold_left
    (check_loc lint_kind suppressions severity_cover)
    (Err, LocSet.empty, unused, true)
    locs

let check err (suppressions: t_map) severity_cover (unused: t_map) =
  let locs =
    Errors.locs_of_error err
    (* It is possible for errors to contain locations without a source, but suppressions always
     * exist in an actual file so there is no point checking if suppressions exist at locations
     * without a source. *)
    |> List.filter (fun {Loc.source; _} -> Option.is_some source)
  in
  (* Ignore lint errors which were never enabled in the first place. *)
  let lint_kind, ignore =
    match Errors.kind_of_error err with
      | Errors.LintError kind ->
        let severity, is_explicit = List.fold_left (fun (s, e) loc ->
          let lint_settings = lint_settings_at_loc loc severity_cover in
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
let all_locs { suppressions; lint_suppressions } =
  suppressions
  |> SpanMap.values
  |> List.fold_left LocSet.union lint_suppressions

let all_locs_of_map map =
  map
  |> FilenameMap.values
  |> List.map all_locs
  |> List.fold_left LocSet.union LocSet.empty
  |> LocSet.elements

let is_empty { suppressions; lint_suppressions } =
  SpanMap.is_empty suppressions && LocSet.is_empty lint_suppressions

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

let update_suppressions current_suppressions new_suppressions =
  FilenameMap.fold begin fun file file_suppressions acc ->
    if is_empty file_suppressions
      then FilenameMap.remove file acc
      else FilenameMap.add file file_suppressions acc
  end new_suppressions current_suppressions
