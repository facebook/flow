(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type t = { subpaths: Export_condition_map.t SMap.t }

let empty = { subpaths = SMap.empty }

let create ~subpaths = { subpaths }

let create_shorthand_index_exports ~export_path =
  let condition_map = Export_condition_map.create_from_shorthand ~path:export_path in
  let subpaths = SMap.empty |> SMap.add "." condition_map in
  create ~subpaths

let pattern_key_compare a b =
  let base_length_a =
    try String.index_from a 0 '*' with
    | Not_found -> -1
  in
  let base_length_b =
    try String.index_from b 0 '*' with
    | Not_found -> -1
  in
  if base_length_a > base_length_b then
    -1
  else if base_length_b > base_length_a then
    1
  else
    let length_a = String.length a in
    let length_b = String.length b in
    if length_a > length_b then
      -1
    else if length_b > length_a then
      1
    else
      0

let rec resolve_matched_wildcard_path subpath_map conditions match_key = function
  | expansion_key :: rest ->
    let pattern_base_index = String.index_from expansion_key 0 '*' in
    let pattern_base = String.sub expansion_key 0 pattern_base_index in
    if String.starts_with ~prefix:pattern_base match_key && match_key <> pattern_base then
      let pattern_trailer = Str.string_after expansion_key (pattern_base_index + 1) in
      if
        String.length pattern_trailer = 0
        || String.ends_with ~suffix:pattern_trailer match_key
           && String.length match_key >= String.length expansion_key
      then
        let target = SMap.find expansion_key subpath_map in
        let pattern_match =
          String.sub
            match_key
            (String.length pattern_base)
            (String.length match_key - String.length pattern_trailer - String.length pattern_base)
        in
        Export_condition_map.resolve_package_target target (Some pattern_match) conditions
      else
        resolve_matched_wildcard_path subpath_map conditions match_key rest
    else
      resolve_matched_wildcard_path subpath_map conditions match_key rest
  | [] -> None

let resolve_wildcard_package exports subpath conditions =
  let expansion_keys =
    SMap.bindings exports.subpaths
    |> List.map (fun (key, _) -> key)
    |> List.filter (fun key -> String.contains key '*')
    |> List.sort pattern_key_compare
  in
  resolve_matched_wildcard_path exports.subpaths conditions subpath expansion_keys

let resolve_package exports subpath conditions =
  match SMap.find_opt subpath exports.subpaths with
  | Some condition_map -> Export_condition_map.resolve_package_target condition_map None conditions
  | None -> resolve_wildcard_package exports subpath conditions

(** Given a list of JSON properties, loosely extract the properties and turn it into a
    [Expression.t SMap.t]. We aren't looking to validate the file, and don't currently
    care about any non-literal properties, so we skip over everything else. *)
let extract_property map property =
  let open Ast in
  let open Expression.Object in
  match property with
  | Property
      ( _,
        Property.Init
          {
            key = Property.StringLiteral (_, { StringLiteral.value = key; _ });
            value = (_, value);
            _;
          }
      ) ->
    SMap.add key value map
  | _ -> map

let collect_export_paths input_path value subpaths_map =
  match Export_condition_map.parse value with
  | Some condition_map -> SMap.add input_path condition_map subpaths_map
  | None -> subpaths_map

let parse_package_exports_obj properties =
  let prop_map = List.fold_left extract_property SMap.empty properties in
  let subpaths = SMap.fold collect_export_paths prop_map SMap.empty in
  create ~subpaths

let parse_conditional_exports_main_sugar obj =
  match Export_condition_map.parse obj with
  | Some condition_map ->
    let subpaths = SMap.empty |> SMap.add "." condition_map in
    Some (create ~subpaths)
  | None -> None

let is_conditional_exports_main_sugar properties =
  let is_path_prop = function
    | Ast.Expression.Object.Property
        ( _,
          Ast.Expression.Object.Property.Init
            {
              key =
                Ast.Expression.Object.Property.StringLiteral
                  (_, { Ast.StringLiteral.value = key; _ });
              _;
            }
        ) ->
      String.starts_with ~prefix:"." key
    | _ -> false
  in
  not (List.exists is_path_prop properties)

let parse expr =
  match expr with
  | Ast.Expression.StringLiteral { Ast.StringLiteral.value : string; _ } ->
    Some (create_shorthand_index_exports ~export_path:value)
  | Ast.Expression.Object obj ->
    let open Ast.Expression.Object in
    if is_conditional_exports_main_sugar obj.properties then
      parse_conditional_exports_main_sugar expr
    else
      Some (parse_package_exports_obj obj.properties)
  | _ -> None
