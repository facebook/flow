(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  projects: string Nel.t;
  projects_overlap_mapping: Bitset.t IMap.t;
  projects_path_mapping: (Str.regexp * Bitset.t) list;
  projects_strict_boundary: bool;
  multi_platform_ambient_supports_platform_project_overrides: (Bitset.t * string list) list;
}

type t = Bitset.t

let equal = Bitset.equal

let index_of ~projects n =
  Base.Option.value_exn (List.find_index (String.equal n) (Nel.to_list projects))

let default_options =
  {
    projects = Nel.one "default";
    projects_overlap_mapping = IMap.empty;
    projects_path_mapping = [];
    projects_strict_boundary = false;
    multi_platform_ambient_supports_platform_project_overrides = [];
  }

let mk_options =
  let list_to_bitset ~projects =
    Base.List.fold
      ~init:(Bitset.all_zero (Nel.length projects))
      ~f:(fun bitset n -> Bitset.set (index_of ~projects n) bitset)
  in
  fun ~projects
      ~projects_overlap_mapping
      ~map_path
      ~projects_path_mapping
      ~projects_strict_boundary
      ~multi_platform_ambient_supports_platform_project_overrides ->
    let projects_overlap_mapping =
      SMap.fold
        (fun k ns acc ->
          IMap.add (index_of ~projects k) (list_to_bitset ~projects (SSet.elements ns)) acc)
        projects_overlap_mapping
        IMap.empty
    in
    let projects_path_mapping =
      Base.List.map projects_path_mapping ~f:(fun (path, ns) ->
          (map_path path, list_to_bitset ~projects ns)
      )
    in
    let multi_platform_ambient_supports_platform_project_overrides =
      (* Given config
       * ```
       * experimental.multi_platform.ambient_supports_platform.project_overrides='web_project' -> 'web'
       * experimental.multi_platform.ambient_supports_platform.project_overrides='native_project' -> 'native'
       * ```
       * and knowing that web project and native project can overlap,
       * we want to generate a mapping of
       *
       * {web_project: [web], native_project: [native], web_project+native_project: [web, native]}
       *)
      let size = Nel.length projects in
      let single_project_multi_platform_ambient_supports_platform_project_overrides =
        (* This part computes the mapping for singleton projects first. *)
        Base.List.map
          multi_platform_ambient_supports_platform_project_overrides
          ~f:(fun (project_str, platforms) ->
            let i = index_of ~projects project_str in
            let project = Bitset.set i (Bitset.all_zero size) in
            (i, project, platforms)
        )
      in
      let composite_project_multi_platform_ambient_supports_platform_project_overrides =
        (* This part uses projects_overlap_mapping map to find out the key of composite project
         * to put into map. *)
        Base.List.fold_right
          single_project_multi_platform_ambient_supports_platform_project_overrides
          ~init:[]
          ~f:(fun (i, _project, platforms) acc ->
            match IMap.find_opt i projects_overlap_mapping with
            | None -> acc
            | Some composite_project ->
              (match Base.List.Assoc.find acc ~equal:Bitset.equal composite_project with
              | None -> (composite_project, platforms) :: acc
              | Some existing_platforms ->
                Base.List.Assoc.add
                  acc
                  ~equal:Bitset.equal
                  composite_project
                  (platforms @ existing_platforms))
        )
      in
      Base.List.map
        single_project_multi_platform_ambient_supports_platform_project_overrides
        ~f:(fun (_i, project, platforms) -> (project, platforms)
      )
      @ composite_project_multi_platform_ambient_supports_platform_project_overrides
    in
    {
      projects;
      projects_overlap_mapping;
      projects_path_mapping;
      projects_strict_boundary;
      multi_platform_ambient_supports_platform_project_overrides;
    }

let from_bitset_unchecked v = v

let to_bitset v = v

let bitset_of_project_string ~opts project =
  Bitset.all_zero (Nel.length opts.projects) |> Bitset.set (index_of ~projects:opts.projects project)

let projects_bitset_of_path ~opts path =
  match opts.projects with
  | (_, []) -> Some (Bitset.all_one 1)
  | (_, _ :: _) ->
    let path = Sys_utils.normalize_filename_dir_sep path in
    (match
       Base.List.find opts.projects_path_mapping ~f:(fun (r, _) -> Str.string_match r path 0)
     with
    | Some (_, bitset) -> Some bitset
    | None -> None)

let is_common_code_path ~opts path =
  match projects_bitset_of_path ~opts path with
  | None -> false
  | Some projects_bitset ->
    IMap.exists
      (fun _ common_project_bitset -> Bitset.equal common_project_bitset projects_bitset)
      opts.projects_overlap_mapping

let multi_platform_ambient_supports_platform_for_project ~opts p =
  let p =
    if opts.projects_strict_boundary then
      p
    else if IMap.exists (fun _ b -> Bitset.equal b p) opts.projects_overlap_mapping then
      (* Under non-strict project boundaries, reduce the common code's platform list into the
       * first concrete project's platform list.
       *
       * This is helpful for a flowconfig for a specific platform (e.g. web) that includes only
       * platform specific code and common code, so that even the common code is assumed to have
       * only one set of platform (e.g. web, instead of web+native) *)
      let size = Nel.length opts.projects in
      Base.List.find_mapi (Nel.to_list opts.projects) ~f:(fun i _ ->
          if Bitset.mem i p then
            Some (Bitset.all_zero size |> Bitset.set i)
          else
            None
      )
      |> Base.Option.value ~default:p
    else
      p
  in
  Base.List.find_map
    opts.multi_platform_ambient_supports_platform_project_overrides
    ~f:(fun (project, platforms) ->
      if Bitset.equal project p then
        Some platforms
      else
        None
  )
