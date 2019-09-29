(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig.With_Loc

let if_one_return_other x a b =
  if x = a then
    Some b
  else if x = b then
    Some a
  else
    None

let find_related_symbol_from_export loc = function
  | (_, ExportDefault { default_loc; local = Some (local, _); _ }) ->
    if_one_return_other loc default_loc local
  | (_, ExportNamed { loc = remote_name_loc; kind }) ->
    begin
      match kind with
      | NamedSpecifier { local = (local_loc, _); _ } ->
        if_one_return_other loc remote_name_loc local_loc
      | NamedDeclaration ->
        if loc = remote_name_loc then
          Some remote_name_loc
        else
          None
    end
  | _ -> None

let find_related_symbol_from_module_kind loc = function
  | CommonJS _ -> None
  | ES { named; _ } ->
    let exports = Core_list.map ~f:snd named in
    ListUtils.first_some_map (find_related_symbol_from_export loc) exports

let rec find_related_symbol_from_bindings loc remote_loc bindings =
  match bindings with
  | BindIdent (local_loc, _) -> if_one_return_other loc remote_loc local_loc
  | BindNamed named ->
    let loc_records (* list of related loc *) =
      List.fold_left
        (fun acc ((remote_loc, _), bindings) ->
          find_related_symbol_from_bindings loc remote_loc bindings :: acc)
        []
        named
    in
    loc_records |> ListUtils.first_some_map (fun x -> x)

let find_related_symbol_from_require loc = function
  | Import { named; _ } ->
    let loc_records (* list of {remote_loc, local_loc} *) =
      SMap.fold
        begin
          fun _ local_name_to_locs acc ->
          SMap.fold
            begin
              fun _ locs acc -> List.rev_append (Nel.to_list locs) acc
            end
            local_name_to_locs
            acc
        end
        named
        []
    in
    loc_records
    |> ListUtils.first_some_map (fun { remote_loc; local_loc } ->
           if_one_return_other loc remote_loc local_loc)
  | Require { bindings = Some bindings; require_loc; _ } ->
    find_related_symbol_from_bindings loc require_loc bindings
  | _ -> None

let find_related_symbol_from_requires loc requires =
  ListUtils.first_some_map (find_related_symbol_from_require loc) requires

let find_related_symbol file_sig loc =
  match find_related_symbol_from_module_kind loc file_sig.module_sig.module_kind with
  | Some _ as result -> result
  | None -> find_related_symbol_from_requires loc file_sig.module_sig.requires

let find_related_symbols file_sig starting_locs =
  Core_list.map ~f:(find_related_symbol file_sig) starting_locs |> ListUtils.cat_maybes
