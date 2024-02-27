(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Bitset.t [@@deriving show]

let available_platforms_to_bitset ~multi_platform_extensions available_platforms =
  let bitset = Bitset.all_zero (Base.List.length multi_platform_extensions) in
  Base.List.iteri multi_platform_extensions ~f:(fun i ext ->
      if Base.List.exists available_platforms ~f:(fun p -> ext = "." ^ p) then Bitset.set bitset i
  );
  bitset

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
      | None ->
        let match_directory_override (path, platform) =
          let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
          Base.Option.some_if (Base.String.is_prefix ~prefix:path normalized_filename) platform
        in
        (match
           Base.List.find_map
             file_options.multi_platform_ambient_supports_platform_directory_overrides
             ~f:match_directory_override
         with
        | None -> Some (Bitset.all_one (Base.List.length multi_platform_extensions))
        | Some supported_platforms_from_directory_overrides ->
          Some
            (available_platforms_to_bitset
               ~multi_platform_extensions
               supported_platforms_from_directory_overrides
            ))
      | Some explicit_available_platforms ->
        Some (available_platforms_to_bitset ~multi_platform_extensions explicit_available_platforms))

let is_subset = Bitset.is_subset

let no_overlap = Bitset.no_overlap

let to_platform_string_set ~file_options bitset =
  Base.List.foldi file_options.Files.multi_platform_extensions ~init:SSet.empty ~f:(fun i acc ext ->
      if Bitset.mem i bitset then
        SSet.add (Base.String.chop_prefix_exn ~prefix:"." ext) acc
      else
        acc
  )

let platform_specific_implementation_mrefs_of_possibly_interface_file
    ~file_options ~platform_set ~file =
  let open Files in
  if file_options.multi_platform && has_flow_ext file then
    let file = chop_flow_ext file in
    let platform_set = Base.Option.value_exn platform_set in
    Base.List.find_map file_options.module_file_exts ~f:(fun module_file_ext ->
        if File_key.check_suffix file module_file_ext then
          let base =
            File_key.chop_suffix file module_file_ext |> File_key.to_string |> Filename.basename
          in
          let implementation_mrefs =
            Base.List.filter_mapi file_options.multi_platform_extensions ~f:(fun i platform_ext ->
                if Bitset.mem i platform_set then
                  Some ("./" ^ base ^ platform_ext)
                else
                  None
            )
          in
          Some implementation_mrefs
        else
          None
    )
  else
    None
