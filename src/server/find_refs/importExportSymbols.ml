(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig

let find_related_symbol_from_export loc = function
  | ExportDefault {default_loc; local=Some(local, _)} ->
    if loc = default_loc then
      Some local
    else if loc = local then
      Some default_loc
    else
      None
  | _ -> None

let find_related_symbol file_sig loc =
  match file_sig.module_sig.module_kind with
  | CommonJS _ -> None
  | ES {named; _} ->
    let exports = SMap.values named in
    ListUtils.first_some_map (find_related_symbol_from_export loc) exports

let find_related_symbols file_sig starting_locs =
  List.map (find_related_symbol file_sig) starting_locs
  |> ListUtils.cat_maybes
