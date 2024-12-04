(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  namespaces: string Nel.t;
  overlapping_namespaces_mapping: Bitset.t IMap.t;
  namespaces_path_mapping: (Str.regexp * Bitset.t) list;
}

type t = Bitset.t

let index_of ~haste_namespaces n =
  Base.Option.value_exn (List.find_index (String.equal n) (Nel.to_list haste_namespaces))

let mk_options =
  let list_to_bitset ~haste_namespaces =
    Base.List.fold
      ~init:(Bitset.all_zero (Nel.length haste_namespaces))
      ~f:(fun bitset n -> Bitset.set (index_of ~haste_namespaces n) bitset)
  in
  fun ~haste_namespaces
      ~haste_overlapping_namespaces_mapping
      ~map_path
      ~haste_namespaces_path_mapping ->
    let overlapping_namespaces_mapping =
      SMap.fold
        (fun k ns acc ->
          IMap.add
            (index_of ~haste_namespaces k)
            (list_to_bitset ~haste_namespaces (SSet.elements ns))
            acc)
        haste_overlapping_namespaces_mapping
        IMap.empty
    in
    let namespaces_path_mapping =
      Base.List.map haste_namespaces_path_mapping ~f:(fun (path, ns) ->
          (map_path path, list_to_bitset ~haste_namespaces ns)
      )
    in
    { namespaces = haste_namespaces; overlapping_namespaces_mapping; namespaces_path_mapping }

let from_bitset_unchecked v = v

let to_bitset v = v

let index_of_namespace_string ~opts = index_of ~haste_namespaces:opts.namespaces

let namespaces_bitset_of_path ~opts path =
  match opts.namespaces with
  | (_, []) -> Bitset.all_one 1
  | (_, _ :: _) ->
    (match
       Base.List.find opts.namespaces_path_mapping ~f:(fun (r, _) -> Str.string_match r path 0)
     with
    | Some (_, bitset) -> bitset
    | None -> failwith ("Path " ^ path ^ " doesn't match any Haste namespace."))

let reachable_namespace_bitsets_from_namespace_bitset ~opts ns =
  let size = Nel.length opts.namespaces in
  let additional =
    Base.List.find_mapi (Nel.to_list opts.namespaces) ~f:(fun i _ ->
        if Bitset.equal ns (Bitset.set i (Bitset.all_zero size)) then
          IMap.find_opt i opts.overlapping_namespaces_mapping
        else
          None
    )
  in
  let additional =
    match additional with
    | Some _ -> additional
    | None ->
      if IMap.exists (fun _ b -> Bitset.equal b ns) opts.overlapping_namespaces_mapping then
        Base.List.find_mapi (Nel.to_list opts.namespaces) ~f:(fun i _ ->
            if Bitset.mem i ns then
              Some (Bitset.all_zero size |> Bitset.set i)
            else
              None
        )
      else
        None
  in
  match additional with
  | Some ns' -> [ns; ns']
  | None -> [ns]
