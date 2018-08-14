(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig

let if_one_return_other x a b =
  if x = a then Some b
  else if x = b then Some a
  else None

let find_related_symbol_from_export loc = function
  | ExportDefault {default_loc; local=Some(local, _)} ->
    if_one_return_other loc default_loc local
  | ExportNamed {loc=remote_name_loc; local; _} ->
    begin match local with
    | Some (local_loc, _) -> if_one_return_other loc remote_name_loc local_loc
    | None when loc = remote_name_loc -> Some remote_name_loc
    | None -> None
    end
  | _ -> None

let find_related_symbol_from_module_kind loc = function
  | CommonJS _ -> None
  | ES {named; _} ->
    let exports = SMap.values named in
    ListUtils.first_some_map (find_related_symbol_from_export loc) exports

let find_related_symbol_from_require loc = function
  | Import {named; _} ->
    let loc_records (* list of {remote_loc, local_loc} *) =
      SMap.fold begin fun _ local_name_to_locs acc ->
        SMap.fold begin fun _ locs acc ->
          List.rev_append (Nel.to_list locs) acc
        end local_name_to_locs acc
      end named []
    in
    loc_records |> ListUtils.first_some_map begin fun {remote_loc; local_loc} ->
      if_one_return_other loc remote_loc local_loc
    end
  | Require {bindings=Some bindings; require_loc; _} ->
    begin match bindings with
    | BindIdent (id_loc, _) -> if_one_return_other loc require_loc id_loc
    | BindNamed named ->
      let loc_records (* list of {remote_loc, local_loc} *) =
        SMap.fold begin fun _ local_name_to_locs acc ->
          SMap.fold begin fun _ locs acc ->
            List.rev_append (Nel.to_list locs) acc
          end local_name_to_locs acc
        end named []
      in
      loc_records |> ListUtils.first_some_map begin fun {remote_loc; local_loc} ->
        if_one_return_other loc remote_loc local_loc
      end
    end
  | _ -> None

let find_related_symbol_from_requires loc requires =
  ListUtils.first_some_map (find_related_symbol_from_require loc) requires

let find_related_symbol file_sig loc =
  match find_related_symbol_from_module_kind loc file_sig.module_sig.module_kind with
    | Some _ as result -> result
    | None -> find_related_symbol_from_requires loc file_sig.module_sig.requires

let find_related_symbols file_sig starting_locs =
  List.map (find_related_symbol file_sig) starting_locs
  |> ListUtils.cat_maybes
