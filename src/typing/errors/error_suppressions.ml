(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

exception No_source of string

module FileSuppressions : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val add : Loc.t -> t -> t

  val remove : Loc.t -> t -> t

  val union : t -> t -> t

  val add_lint_suppression : Loc.t -> t -> t

  val remove_lint_suppression : Loc.t -> t -> t

  val suppression_at_loc : Loc.t -> t -> LocSet.t option

  val all_locs : t -> LocSet.t
end = struct
  type error_suppressions = LocSet.t SpanMap.t

  type t = {
    suppressions: error_suppressions;
    lint_suppressions: LocSet.t;
  }

  let empty = { suppressions = SpanMap.empty; lint_suppressions = LocSet.empty }

  let is_empty { suppressions; lint_suppressions } =
    SpanMap.is_empty suppressions && LocSet.is_empty lint_suppressions

  let add loc { suppressions; lint_suppressions } =
    let suppression_loc =
      Loc.(
        let start = { line = loc._end.line + 1; column = 0 } in
        let _end = { line = loc._end.line + 2; column = 0 } in
        { loc with start; _end })
    in
    let suppressions =
      SpanMap.add suppression_loc (LocSet.singleton loc) suppressions ~combine:LocSet.union
    in
    { suppressions; lint_suppressions }

  let remove loc ({ suppressions; _ } as orig) =
    { orig with suppressions = SpanMap.remove loc suppressions }

  let union a b =
    {
      suppressions = SpanMap.union a.suppressions b.suppressions;
      lint_suppressions = LocSet.union a.lint_suppressions b.lint_suppressions;
    }

  let add_lint_suppression lint_suppression t =
    { t with lint_suppressions = LocSet.add lint_suppression t.lint_suppressions }

  let remove_lint_suppression lint_suppression ({ lint_suppressions; _ } as orig) =
    { orig with lint_suppressions = LocSet.remove lint_suppression lint_suppressions }

  let suppression_at_loc loc { suppressions; _ } = SpanMap.find_opt loc suppressions

  let all_locs { suppressions; lint_suppressions } =
    suppressions |> SpanMap.values |> List.fold_left LocSet.union lint_suppressions
end

type t = FileSuppressions.t FilenameMap.t

let empty = FilenameMap.empty

let file_of_loc_unsafe loc =
  match loc.Loc.source with
  | Some x -> x
  | None -> raise (No_source (Loc.debug_to_string ~include_source:true loc))

let add loc map =
  let file = file_of_loc_unsafe loc in
  let suppressions = FileSuppressions.empty |> FileSuppressions.add loc in
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
        FilenameMap.find_opt file acc |> Option.value ~default:FileSuppressions.empty
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

let remove_suppression_from_map loc (suppressions_map : t) =
  update_file_suppressions (FileSuppressions.remove loc) loc suppressions_map

let remove_lint_suppression_from_map loc (suppressions_map : t) =
  update_file_suppressions (FileSuppressions.remove_lint_suppression loc) loc suppressions_map

let check_loc suppressions (result, used, (unused : t)) loc =
  (* We only want to check the starting position of the reason *)
  let loc = Loc.first_char loc in
  match suppression_at_loc loc suppressions with
  | Some locs ->
    let used = LocSet.union locs used in
    let unused = remove_suppression_from_map loc unused in
    (Off, used, unused)
  | None -> (result, used, unused)

(* Checks if any of the given locations should be suppressed. *)
let check_locs locs (suppressions : t) (unused : t) =
  (* We need to check every location in order to figure out which suppressions
     are really unused...that's why we don't shortcircuit as soon as we find a
     matching error suppression.
     If the "primary" location has severity = Off, the error should be
     suppressed even if it is not explicit. *)
  List.fold_left (check_loc suppressions) (Err, LocSet.empty, unused) locs

let in_node_modules ~root ~file_options loc =
  match Option.both (Loc.source loc) file_options with
  | None -> false
  | Some (file, options) -> Files.is_within_node_modules ~root ~options (File_key.to_string file)

let in_declarations ~file_options loc =
  match Option.both (Loc.source loc) file_options with
  | None -> false
  | Some (file, options) -> Files.is_declaration options (File_key.to_string file)

let check ~root ~file_options (err : Loc.t Errors.printable_error) (suppressions : t) (unused : t) =
  let locs =
    Errors.locs_of_printable_error err
    (* It is possible for errors to contain locations without a source, but suppressions always
     * exist in an actual file so there is no point checking if suppressions exist at locations
     * without a source. *)
    |> List.filter (fun loc -> Option.is_some (Loc.source loc))
  in
  (* Ignore lint errors from node modules, and all errors from declarations directories. *)
  let ignore =
    match Errors.kind_of_printable_error err with
    | Errors.LintError _ -> in_node_modules ~root ~file_options (Errors.loc_of_printable_error err)
    | _ -> in_declarations ~file_options (Errors.loc_of_printable_error err)
  in
  if ignore then
    None
  else
    let (result, used, unused) = check_locs locs suppressions unused in
    let result =
      match Errors.kind_of_printable_error err with
      | Errors.RecursionLimitError ->
        (* TODO: any related suppressions should not be considered used *)
        Err
      | _ -> result
    in
    Some (result, used, unused)

(* Gets the locations of the suppression comments that are yet unused *)

let all_locs map =
  FilenameMap.fold (fun _k v acc -> LocSet.union acc (FileSuppressions.all_locs v)) map LocSet.empty

let filter_suppressed_errors ~root ~file_options suppressions errors ~unused =
  (* Filter out suppressed errors. also track which suppressions are used. *)
  Errors.ConcreteLocPrintableErrorSet.fold
    (fun error ((errors, suppressed, unused) as acc) ->
      match check ~root ~file_options error suppressions unused with
      | None -> acc
      | Some (severity, used, unused) ->
        (match severity with
        | Off -> (errors, (error, used) :: suppressed, unused)
        | _ -> (Errors.ConcreteLocPrintableErrorSet.add error errors, suppressed, unused)))
    errors
    (Errors.ConcreteLocPrintableErrorSet.empty, [], unused)

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
  Option.Monad_infix.(
    Loc.source loc >>= fun source ->
    Utils_js.FilenameMap.find_opt source severity_cover >>= ExactCover.find_opt loc)

(* Filter out lint errors which are definitely suppressed or were never
 * enabled in the first place. *)
let filter_lints suppressions errors aloc_tables ~include_suppressions severity_cover =
  Flow_error.(
    ErrorSet.fold
      (fun error (errors, warnings, suppressions) ->
        Severity.(
          match (msg_of_error error |> Error_message.kind_of_msg, loc_of_error error) with
          | (Errors.LintError lint_kind, Some loc) ->
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
          | _ -> (ErrorSet.add error errors, warnings, suppressions)))
      errors
      (ErrorSet.empty, ErrorSet.empty, suppressions))
