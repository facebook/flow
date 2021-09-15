(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lints
open Severity

let ( >>= ) = Base.Result.( >>= )

type 'a t = {
  (* The default value associated with a lint if the lint kind isn't found in the map *)
  default_value: 'a;
  (* Values for lints that have been explicitly set *)
  (* The Loc.t is for settings defined in comments, and is used to find unused lint
   * suppressions. The Loc.t is set to None for settings coming from the flowconfig or --lints.*)
  explicit_values: ('a * Loc.t option) LintMap.t;
}

type warning = int * string

type error = int * string

let default_explicit_values =
  LintMap.singleton Lints.DeprecatedUtility (Severity.Err, None)
  |> LintMap.add Lints.UntypedTypeImport (Severity.Err, None)

let ignored_by_all =
  [
    Lints.DeprecatedUtility;
    Lints.ImplicitInexactObject;
    Lints.AmbiguousObjectType;
    Lints.UninitializedInstanceProperty;
  ]

let config_default kind =
  LintMap.find_opt kind default_explicit_values |> Base.Option.value ~default:(Severity.Off, None)

let of_default default_value =
  let explicit_values = LintMap.of_function ignored_by_all config_default in
  { default_value; explicit_values }

let set_value key value settings =
  let new_map = LintMap.add key value settings.explicit_values in
  { settings with explicit_values = new_map }

let set_all entries settings =
  List.fold_left (fun settings (key, value) -> set_value key value settings) settings entries

let get_default settings = settings.default_value

let get_value lint_kind settings =
  LintMap.find_opt lint_kind settings.explicit_values
  |> Base.Option.value_map ~f:fst ~default:settings.default_value

let get_loc lint_kind settings =
  LintMap.find_opt lint_kind settings.explicit_values |> Base.Option.value_map ~f:snd ~default:None

let is_explicit lint_kind settings = LintMap.mem lint_kind settings.explicit_values

(* Iterate over all lint kinds with an explicit value *)
let iter f settings = LintMap.iter f settings.explicit_values

(* Fold over all lint kinds with an explicit value *)
let fold f settings acc = LintMap.fold f settings.explicit_values acc

(* Map over all lint kinds with an explicit value *)
let map f settings =
  let new_explicit = LintMap.map f settings.explicit_values in
  { settings with explicit_values = new_explicit }

(* SEVERITY-SPECIFIC FUNCTIONS *)

let empty_severities = { default_value = Off; explicit_values = LintMap.empty }

let default_severities = { empty_severities with explicit_values = default_explicit_values }

let is_enabled lint_kind settings =
  match get_value lint_kind settings with
  | Err
  | Warn ->
    true
  | Off -> false

let is_suppressed lint_kind settings = is_enabled lint_kind settings |> not

type parse_result =
  | AllSetting of severity t
  | EntryList of lint_kind list * (severity * Loc.t option)

(* Takes a base LintSettings and a list of labeled lines and returns the corresponding
 * severity LintSettings.t from applying the new lines on top of the base settings if
 * successful. Otherwise, returns an error message along with the label of the
 * line it failed on. *)
let of_lines base_settings =
  let parse_value label value =
    match severity_of_string value with
    | Some severity -> Ok severity
    | None -> Error (label, "Invalid setting encountered. Valid settings are error, warn, and off.")
  in
  let eq_regex = Str.regexp "=" in
  let all_regex = Str.regexp "all" in
  let parse_line (loc, (label, line)) =
    match Str.split_delim eq_regex line with
    | [left; right] ->
      let left = left |> String.trim in
      let right = right |> String.trim in
      parse_value label right >>= fun value ->
      begin
        match left with
        | "all" -> Ok (AllSetting (of_default value))
        | _ ->
          begin
            match kinds_of_string left with
            | Some kinds -> Ok (EntryList (kinds, (value, Some loc)))
            | None -> Error (label, Printf.sprintf "Invalid lint rule \"%s\" encountered." left)
          end
      end
    | _ ->
      Error (label, "Malformed lint rule. Properly formed rules contain a single '=' character.")
  in
  let add_value keys value settings =
    let (new_settings, all_redundant) =
      List.fold_left
        (fun (settings, all_redundant) key ->
          let v = get_value key settings in
          let all_redundant = all_redundant && v = fst value && v <> fst (config_default key) in
          let settings = set_value key value settings in
          (settings, all_redundant))
        (settings, true)
        keys
    in
    if all_redundant then
      Error "Redundant argument. This argument doesn't change any lint settings."
    else
      Ok new_settings
  in
  let rec loop (acc : Severity.severity t) (warnings : warning list) = function
    | [] -> Ok (acc, Base.List.rev warnings)
    | line :: lines ->
      (match parse_line line with
      | Ok (EntryList (keys, value)) ->
        (match add_value keys value acc with
        | Ok acc -> loop acc warnings lines
        | Error msg ->
          let warning = (line |> snd |> fst, msg) in
          loop acc (warning :: warnings) lines)
      | Ok (AllSetting value) ->
        if acc == base_settings then
          loop value warnings lines
        else
          Error
            ( line |> snd |> fst,
              "\"all\" is only allowed as the first setting. Settings are order-sensitive." )
      | Error line_err -> loop acc (line_err :: warnings) lines)
  in
  let loc_of_line line =
    Loc.(
      let start = { line; column = 0 } in
      let _end = { line = line + 1; column = 0 } in
      { source = None; start; _end })
  in
  fun lint_lines ->
    let locate_fun ((label, _) as item) = (loc_of_line label, item) in
    (* Artificially locate the lines to detect unused lines *)
    let located_lines = Base.List.map ~f:locate_fun lint_lines in
    loop base_settings [] located_lines >>= fun (settings, warnings) ->
    let used_locs =
      fold
        (fun _kind (_enabled, loc) acc ->
          Base.Option.value_map loc ~f:(fun loc -> Loc_collections.LocSet.add loc acc) ~default:acc)
        settings
        Loc_collections.LocSet.empty
    in
    let used_locs =
      Base.List.fold
        ~f:(fun acc (line, _warning) -> Loc_collections.LocSet.add (loc_of_line line) acc)
        ~init:used_locs
        warnings
    in
    let first_unused =
      List.fold_left
        (fun acc (art_loc, (label, line)) ->
          match acc with
          | Some _ -> acc
          | None ->
            if
              Loc_collections.LocSet.mem art_loc used_locs
              || Str.string_match all_regex (String.trim line) 0
            then
              None
            else
              Some label)
        None
        located_lines
    in
    let warnings =
      match first_unused with
      | Some label ->
        let warning =
          ( label,
            "Redundant argument. " ^ "The values set by this argument are completely overwritten."
          )
        in
        warning :: warnings
      | None -> warnings
    in
    (* Remove the artificial locations before returning the result *)
    let settings = map (fun (enabled, _loc) -> (enabled, None)) settings in
    Ok (settings, warnings)

let to_string settings =
  let acc = Buffer.create 20 in
  Buffer.add_string acc (Printf.sprintf "all=%s" (settings |> get_default |> string_of_severity));
  iter
    (fun kind (severity, _) ->
      Buffer.add_string
        acc
        (Printf.sprintf ", %s=%s" (string_of_kind kind) (string_of_severity severity)))
    settings;
  Buffer.contents acc

type lint_parse_error =
  | Invalid_setting
  | Malformed_argument
  | Naked_comment
  | Nonexistent_rule
  | Overwritten_argument
  | Redundant_argument
