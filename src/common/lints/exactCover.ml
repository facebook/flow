(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* See exactCover.mli for a high-level explanation. *)

exception Uncovered of string

(* A builder is a sorted list of non-overlapping, non-empty, adjacent ranges,
 * with later ranges appearing at the head of the list and earlier ranges
 * appearing at the tail of the list. *)
type 'a builder = (Loc.t * 'a) list

let new_builder =
  Loc.(
    let full_range source =
      let start = { line = 0; column = 0 } in
      let _end = { line = max_int / 2; column = max_int / 2 } in
      { source; start; _end }
    in
    (fun source value -> [(full_range (Some source), value)]))

(* Gets all ranges that intersect with the provided range. *)
let get_intersecting =
  let rec get_intersecting' query acc = function
    | [] -> acc (* We've exhausted the list. *)
    | candidate :: tail ->
      let cand_range = fst candidate in
      if Loc.(pos_cmp query._end cand_range.start) <= 0 then
        (* We still haven't reached the first intersecting range. *)
        get_intersecting' query acc tail
      else if Loc.(pos_cmp cand_range._end query.start) > 0 then
        (* The current range is intersecting. *)
        get_intersecting' query (candidate :: acc) tail
      else
        (* We've passed the last intersecting range. *)
        acc
  in
  (fun range builder -> get_intersecting' range [] builder)

(* Adds the provided entry to the builder. (Assumes that there is no range in
 * the builder that overlaps with the provided entry.) *)
let add =
  let rec add' entry visited = function
    | head :: tail as builder ->
      if Loc.compare (fst entry) (fst head) > 0 then
        (* We found the place the entry is supposed to go *)
        List.rev_append visited (entry :: builder)
      else
        (* We need to keep going *)
        add' entry (head :: visited) tail
    | [] -> List.rev_append visited [entry]
  in
  (fun entry builder -> add' entry [] builder)

(* Removes the provided entry from the builder. (Assumes that the provided entry
 * exists in the builder.) *)
let remove =
  let rec remove' entry visited = function
    | head :: tail ->
      (* '==' is acceptable because this is only called on ranges that have been
       * taken out of the builder itself. *)
      if head == entry then
        List.rev_append visited tail
      else
        remove' entry (head :: visited) tail
    | [] -> Utils_js.assert_false "Remove is only called on entries that exist in the builder."
  in
  (fun entry builder -> remove' entry [] builder)

(* Takes two overlapping ranges, old_range and new_range, and returns a tuple
 * (intersection, remaining), where intersection is the intersection of
 * old_range and new_range, and remaining is a list of 0-2 ranges that, when
 * unioned with intersection, form old_range. *)
let get_overlap ~old_range ~new_range =
  Loc.(
    let source = new_range.source in
    let (start, remaining) =
      if pos_cmp old_range.start new_range.start < 0 then
        (new_range.start, [{ source; start = old_range.start; _end = new_range.start }])
      else
        (old_range.start, [])
    in
    let (_end, remaining) =
      if pos_cmp old_range._end new_range._end > 0 then
        (new_range._end, { source; start = new_range._end; _end = old_range._end } :: remaining)
      else
        (old_range._end, remaining)
    in
    ({ source; start; _end }, remaining))

(* Given an entry in a builder, a range to do a modification in the builder, a
 * modification function, and the builder, returns a builder with the range
 * covered by the entry updated according to the modification range and
 * modification function. *)
let update_entry ((old_range, old_value) as original) new_range map_fun builder =
  let (overlap, remaining_ranges) = get_overlap ~old_range ~new_range in
  let new_overlap = (overlap, map_fun old_value) in
  let new_remaining = Core_list.map ~f:(fun loc -> (loc, old_value)) remaining_ranges in
  let builder = builder |> remove original |> add new_overlap in
  List.fold_left (Fn.flip add) builder new_remaining

(* Given a range over which to perform a modification, a modification function,
 * and a builder to work on, returns a builder modified by the modification
 * function over the provided range. *)
let update_range range map_fun builder =
  let original_intersecting = get_intersecting range builder in
  List.fold_left
    (fun builder original -> update_entry original range map_fun builder)
    builder
    original_intersecting

let update_settings =
  let map_fun setting_list old_settings =
    let setting_list =
      Core_list.map ~f:(fun (kind, (state, loc)) -> (kind, (state, Some loc))) setting_list
    in
    LintSettings.set_all setting_list old_settings
  in
  (fun range setting_list builder -> update_range range (map_fun setting_list) builder)

let update_settings_and_running =
  let update_settings_and_error err_fun settings settings_list =
    match settings_list with
    | (_, (_, loc)) :: _ ->
      let (new_settings, all_redundant) =
        List.fold_left
          (fun (settings, all_redundant) (kind, (state, loc)) ->
            let this_redundant = LintSettings.get_value kind settings = state in
            (* Still do set_state to update the location, otherwise it's
             * reported that the results of the argument get overwritten. *)
            let new_settings = LintSettings.set_value kind (state, Some loc) settings in
            (new_settings, all_redundant && this_redundant))
          (settings, true)
          settings_list
      in
      if all_redundant then err_fun (loc, LintSettings.Redundant_argument);
      new_settings
    | [] -> settings
  in
  let update_settings_and_error_from_list err_fun settings_list_list settings =
    List.fold_left (update_settings_and_error err_fun) settings settings_list_list
  in
  fun running_settings err_fun range settings_list_list builder ->
    let flat_settings_list = List.flatten settings_list_list in
    let updated_builder = update_settings range flat_settings_list builder in
    let updated_running_settings =
      update_settings_and_error_from_list err_fun settings_list_list running_settings
    in
    (updated_builder, updated_running_settings)

let bake builder =
  List.fold_left (fun map (loc, value) -> SpanMap.add loc value map) SpanMap.empty builder

(* Supports O(log(n)) queries to get the value associated with a loc. *)
type 'a t = 'a SpanMap.t

let file_cover source value = new_builder source value |> bake

(* Gets the value associated with a certain location in the code. To resolve
 * ambiguity, this looks at the location of the first character in the provided
 * location. Errors if queried for a file not contained in this cover. *)
let find loc cover =
  let first_char = Loc.first_char loc in
  try SpanMap.find_unsafe first_char cover
  with Not_found -> raise (Uncovered (Loc.debug_to_string ~include_source:true loc))

let find_opt loc cover =
  let first_char = Loc.first_char loc in
  SpanMap.get first_char cover

(* `severity LintSettings.t`-specific functions *)

type lint_severity_cover = Severity.severity LintSettings.t t

let default_file_cover source = file_cover source LintSettings.empty_severities

let get_severity lint_kind loc severity_cover =
  find loc severity_cover |> LintSettings.get_value lint_kind

let is_suppressed lint_kind loc severity_cover =
  find loc severity_cover |> LintSettings.is_suppressed lint_kind

let is_explicit lint_kind loc severity_cover =
  find loc severity_cover |> LintSettings.is_explicit lint_kind

let to_string settings =
  let loc_to_str = Loc.debug_to_string ~include_source:true in
  let acc = Buffer.create 100 in
  let () =
    SpanMap.iter
      (fun loc settings ->
        Buffer.add_string
          acc
          (Printf.sprintf "%s: %s\n" (loc_to_str loc) (LintSettings.to_string settings)))
      settings
  in
  (* Strip the trailing newline. *)
  Buffer.sub acc 0 (Buffer.length acc - 1)
