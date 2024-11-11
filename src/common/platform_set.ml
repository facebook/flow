(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Bitset.t [@@deriving show]

let available_platforms_to_bitset ~multi_platform_extensions available_platforms =
  let bitset = Bitset.all_zero (Base.List.length multi_platform_extensions) in
  Base.List.foldi multi_platform_extensions ~init:bitset ~f:(fun i bitset ext ->
      if Base.List.exists available_platforms ~f:(fun p -> ext = "." ^ p) then
        Bitset.set i bitset
      else
        bitset
  )

let available_platforms ~file_options ~filename ~explicit_available_platforms : t option =
  if not (Files.multi_platform file_options) then
    None
  else
    let multi_platform_extensions = Files.multi_platform_extensions file_options in
    match Files.platform_specific_extensions_and_indices_opt ~options:file_options filename with
    | Some result ->
      let bitset = Bitset.all_zero (Base.List.length multi_platform_extensions) in
      let bitset =
        Base.List.fold result ~init:bitset ~f:(fun bitset (i, _) -> Bitset.set i bitset)
      in
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
             (Files.multi_platform_ambient_supports_platform_directory_overrides file_options)
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
  Base.List.foldi
    (Files.multi_platform_extensions file_options)
    ~init:SSet.empty
    ~f:(fun i acc ext ->
      if Bitset.mem i bitset then
        SSet.add (Base.String.chop_prefix_exn ~prefix:"." ext) acc
      else
        acc
  )

let platform_specific_implementation_mrefs_of_possibly_interface_file
    ~file_options ~platform_set ~file =
  if Files.multi_platform file_options && Files.has_flow_ext file then
    let file = Files.chop_flow_ext file in
    let platform_set = Base.Option.value_exn platform_set in
    Base.List.find_map (Files.module_file_exts file_options) ~f:(fun module_file_ext ->
        if File_key.check_suffix file module_file_ext then
          let base =
            File_key.chop_suffix file module_file_ext |> File_key.to_string |> Filename.basename
          in
          let platform_extensions =
            Base.List.filter_mapi
              (Files.multi_platform_extensions file_options)
              ~f:(fun i platform_ext ->
                if Bitset.mem i platform_set then
                  Some platform_ext
                else
                  None
            )
          in
          let multi_platform_extension_group_mapping =
            Files.multi_platform_extension_group_mapping file_options
          in
          let (unconditional_extensions, grouped_extensions_with_conditional_extensions) =
            Base.List.fold
              platform_extensions
              ~init:(SSet.empty, SMap.empty)
              ~f:(fun (unconditional_extensions, grouped_extensions_with_conditional_extensions) ext
                 ->
                match
                  Base.List.find
                    multi_platform_extension_group_mapping
                    ~f:(fun (_group_ext, platforms) ->
                      Base.List.exists platforms ~f:(fun p -> "." ^ p = ext)
                  )
                with
                | None ->
                  ( SSet.add ext unconditional_extensions,
                    grouped_extensions_with_conditional_extensions
                  )
                | Some (group_ext, _) ->
                  ( unconditional_extensions,
                    SMap.adjust
                      group_ext
                      (function
                        | None -> SSet.singleton ext
                        | Some s -> SSet.add ext s)
                      grouped_extensions_with_conditional_extensions
                  )
            )
          in
          let implementation_mref_of_platform_extension platform_ext = "./" ^ base ^ platform_ext in
          Some
            ( unconditional_extensions
              |> SSet.elements
              |> List.map implementation_mref_of_platform_extension,
              grouped_extensions_with_conditional_extensions
              |> SMap.elements
              |> List.map (fun (group_ext, conditional_exts) ->
                     ( implementation_mref_of_platform_extension group_ext,
                       conditional_exts
                       |> SSet.elements
                       |> List.map implementation_mref_of_platform_extension
                     )
                 )
            )
        else
          None
    )
  else
    None
