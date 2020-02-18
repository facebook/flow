(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lints
open Severity
open Utils_js

let ( >>= ) = Base.Result.( >>= )

type 'a t = {
  (* The default value associated with a lint if the lint kind isn't found in the map *)
  default_value: 'a;
  (* Values for lints that have been explicitly set *)
  (* The Loc.t is for settings defined in comments, and is used to find unused lint
   * suppressions. The Loc.t is set to None for settings coming from the flowconfig or --lints.*)
  explicit_values: ('a * Loc.t option) LintMap.t;
}

let default_lint_severities = [(Lints.DeprecatedUtility, (Severity.Err, None))]

let ignored_by_all =
  [
    Lints.DynamicExport;
    Lints.DeprecatedUtility;
    Lints.ImplicitInexactObject;
    Lints.AmbiguousObjectType;
    Lints.UninitializedInstanceProperty;
  ]

let config_default =
  Base.List.Assoc.find default_lint_severities ~equal:( = )
  %> Base.Option.value ~default:(Severity.Off, None)

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
  let rec loop acc = function
    | [] -> Ok acc
    | line :: lines ->
      parse_line line >>= fun result ->
      begin
        match result with
        | EntryList (keys, value) ->
          begin
            match add_value keys value acc with
            | Ok settings -> loop settings lines
            | Error msg -> Error (line |> snd |> fst, msg)
          end
        | AllSetting value ->
          if acc == base_settings then
            loop value lines
          else
            Error
              ( line |> snd |> fst,
                "\"all\" is only allowed as the first setting. Settings are order-sensitive." )
      end
  in
  let loc_of_line line =
    Loc.(
      let start = { line; column = 0 } in
      let _end = { line = line + 1; column = 0 } in
      { source = None; start; _end })
  in
  fun lint_lines ->
    let locate_fun =
      let index = ref 0 in
      fun item ->
        let res = (loc_of_line !index, item) in
        index := !index + 1;
        res
    in
    (* Artificially locate the lines to detect unused lines *)
    let located_lines = Base.List.map ~f:locate_fun lint_lines in
    let settings = loop base_settings located_lines in
    settings >>= fun settings ->
    let used_locs =
      fold
        (fun _kind (_enabled, loc) acc ->
          Base.Option.value_map loc ~f:(fun loc -> Loc_collections.LocSet.add loc acc) ~default:acc)
        settings
        Loc_collections.LocSet.empty
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
    match first_unused with
    | Some label ->
      Error
        ( label,
          "Redundant argument. " ^ "The values set by this argument are completely overwritten." )
    | None ->
      (* Remove the artificial locations before returning the result *)
      Ok (map (fun (enabled, _loc) -> (enabled, None)) settings)

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
