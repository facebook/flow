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
  projects_strict_boundary_validate_import_pattern_opt_outs: bool;
  projects_strict_boundary_import_pattern_opt_outs: Str.regexp list;
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
    projects_strict_boundary_validate_import_pattern_opt_outs = false;
    projects_strict_boundary_import_pattern_opt_outs = [];
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
      ~projects_strict_boundary_validate_import_pattern_opt_outs
      ~projects_strict_boundary_import_pattern_opt_outs
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
      projects_strict_boundary_validate_import_pattern_opt_outs;
      projects_strict_boundary_import_pattern_opt_outs;
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

let projects_strict_boundary_validate_import_pattern_opt_outs ~opts =
  opts.projects_strict_boundary_validate_import_pattern_opt_outs

let is_import_specifier_that_opt_out_of_strict_boundary ~opts ~import_specifier =
  Base.List.exists opts.projects_strict_boundary_import_pattern_opt_outs ~f:(fun pattern ->
      Str.string_match pattern import_specifier 0
  )

(**
 * Suppose we have web and native project, and some paths that can be part of both web and native.
 * Then this function will return which projects' files can be accessed by the given project.
 *
 * This is used to enforce that web code can use both web and web+native code, while web+native code
 * can only import web+native code. However, the latter is temporarily allowed for experimentation.
 *)
let reachable_projects_bitsets_from_projects_bitset ~opts ~import_specifier p =
  let size = Nel.length opts.projects in
  (* 1-project code can reach into common code.
   * e.g. Suppose that we have two projects web and native.
   * Web code can use common code (web+native). *)
  let additional_from_common_code =
    Base.List.find_mapi (Nel.to_list opts.projects) ~f:(fun i _ ->
        if Bitset.equal p (Bitset.set i (Bitset.all_zero size)) then
          IMap.find_opt i opts.projects_overlap_mapping
        else
          None
    )
  in
  (* Normally, we do not allow common code importing one-project code. However, when we decide
   * to allow it for compatibility purposes, we will pick the one project declared first in flowconfig. *)
  let one_project_reachable_from_common_code () =
    if IMap.exists (fun _ b -> Bitset.equal b p) opts.projects_overlap_mapping then
      Base.List.find_mapi (Nel.to_list opts.projects) ~f:(fun i _ ->
          if Bitset.mem i p then
            Some (Bitset.all_zero size |> Bitset.set i)
          else
            None
      )
    else
      None
  in
  (* Unlike the one below, these imports from common code into 1-project code is explicitly allowed.
   * To maintain sanity, we will ensure that the files from different namespaces will have the same
   * signature as the chosen one here. *)
  let additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs =
    match
      ( additional_from_common_code,
        is_import_specifier_that_opt_out_of_strict_boundary ~opts ~import_specifier
      )
    with
    | (Some _, _)
    | (_, false) ->
      None
    | (None, true) -> one_project_reachable_from_common_code ()
  in
  let additional_from_1_project_code_unsafe =
    if opts.projects_strict_boundary then
      None
    else
      (* Temporary hack: common code can reach into 1-project code.
       * e.g. Suppose that we have two projects web and native.
       * We temporarily allow common code (web+native) to use web-only code.
       * This is of course incorrect, and we should move these web-only code into common code instead.
       * However, the temporary measure exists so that we can still have good type coverage during
       * experimentation before we can lock down the boundary. *)
      match
        ( additional_from_common_code,
          additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs
        )
      with
      | (Some _, _)
      | (_, Some _) ->
        None
      | (None, None) -> one_project_reachable_from_common_code ()
  in
  p
  :: (Base.Option.to_list additional_from_common_code
     @ Base.Option.to_list
         additional_from_1_project_code_allowed_with_strict_boundary_import_pattern_opt_outs
     @ Base.Option.to_list additional_from_1_project_code_unsafe
     )

(**
 * Suppose we have web and native project, and some paths that can be part of both web and native.
 * Then this function will always return the bitset representation of web,native projects, if and
 * only if it's given a web+native project.
 *
 * This is important to enforce in Haste that we have only one provider for a module name `N`. The
 * module name might come from multiple projects, so we have to search for files that might provide
 * all of the following haste_module_info: `web:N`, `native:N` to ensure that there is no module
 * that will also provide `N` that's already provided in the common code.
 *)
let individual_projects_bitsets_from_common_project_bitset ~opts common =
  let size = Nel.length opts.projects in
  if Base.List.mem ~equal:Bitset.equal (IMap.values opts.projects_overlap_mapping) common then
    (* Given the common project, compute all the individual singleton projects in it. *)
    let individual_singleton_projects =
      if IMap.exists (fun _ b -> Bitset.equal b common) opts.projects_overlap_mapping then
        Base.List.filter_mapi (Nel.to_list opts.projects) ~f:(fun i _ ->
            if Bitset.mem i common then
              Some (Bitset.all_zero size |> Bitset.set i)
            else
              None
        )
      else
        []
    in
    Some individual_singleton_projects
  else
    None

(* Same as above, but drop the first project.
 * This is useful for projects_strict_boundary_import_pattern_opt_outs, where we pick the file from
 * the first project to act as common interface, but then we validate that all the other corresponding
 * files in other projects conform to the interface of the first file.
 *)
let individual_projects_bitsets_from_common_project_bitset_excluding_first ~opts common =
  match individual_projects_bitsets_from_common_project_bitset ~opts common with
  | None -> None
  | Some [] -> None
  | Some (_fst :: rest) -> Some rest

let multi_platform_ambient_supports_platform_for_project ~opts p =
  let p =
    if opts.projects_strict_boundary then
      p
    else if IMap.exists (fun _ b -> Bitset.equal b p) opts.projects_overlap_mapping then
      (* Similar to the temporary hack above in `reachable_projects_bitsets_from_projects_bitset`
       * that allows common code to reach into 1-project code,
       * here we make reduce the common code's platform list into the 1-project's platform list.
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
