(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Bitset.t [@@deriving show]

let available_platforms ~file_options ~filename ~explicit_available_platforms : t option =
  let open Files in
  if not file_options.multi_platform then
    None
  else
    let multi_platform_extensions = file_options.multi_platform_extensions in
    match platform_specific_extension_and_index_opt ~options:file_options filename with
    | Some (i, _) ->
      let bitset = Bitset.all_zero (Base.List.length multi_platform_extensions) in
      Bitset.set bitset i;
      Some bitset
    | None ->
      (match explicit_available_platforms with
      | None -> Some (Bitset.all_one (Base.List.length multi_platform_extensions))
      | Some explicit_available_platforms ->
        let bitset = Bitset.all_zero (Base.List.length multi_platform_extensions) in
        Base.List.iteri multi_platform_extensions ~f:(fun i ext ->
            if Base.List.exists explicit_available_platforms ~f:(fun p -> ext = "." ^ p) then
              Bitset.set bitset i
        );
        Some bitset)

let is_subset = Bitset.is_subset

let no_overlap = Bitset.no_overlap

let to_platform_string_set ~file_options bitset =
  Base.List.foldi file_options.Files.multi_platform_extensions ~init:SSet.empty ~f:(fun i acc ext ->
      if Bitset.mem i bitset then
        SSet.add (Base.String.chop_prefix_exn ~prefix:"." ext) acc
      else
        acc
  )
