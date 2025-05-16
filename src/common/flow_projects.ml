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
  }

let mk_options =
  let list_to_bitset ~projects =
    Base.List.fold
      ~init:(Bitset.all_zero (Nel.length projects))
      ~f:(fun bitset n -> Bitset.set (index_of ~projects n) bitset)
  in
  fun ~projects ~projects_overlap_mapping ~map_path ~projects_path_mapping ~projects_strict_boundary ->
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
    { projects; projects_overlap_mapping; projects_path_mapping; projects_strict_boundary }

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

(**
 * Suppose we have web and native project, and some paths that can be part of both web and native.
 * Then this function will return which projects' files can be accessed by the given project.
 *
 * This is used to enforce that web code can use both web and web+native code, while web+native code
 * can only import web+native code. However, the latter is temporarily allowed for experimentation.
 *)
let reachable_projects_bitsets_from_projects_bitset ~opts p =
  let size = Nel.length opts.projects in
  (* 1-project code can reach into common code.
   * e.g. Suppose that we have two projects web and native.
   * Web code can use common code (web+native). *)
  let additional =
    Base.List.find_mapi (Nel.to_list opts.projects) ~f:(fun i _ ->
        if Bitset.equal p (Bitset.set i (Bitset.all_zero size)) then
          IMap.find_opt i opts.projects_overlap_mapping
        else
          None
    )
  in
  let additional =
    if opts.projects_strict_boundary then
      additional
    else
      (* Temporary hack: common code can reach into 1-project code.
       * e.g. Suppose that we have two projects web and native.
       * We temporarily allow common code (web+native) to use web-only code.
       * This is of course incorrect, and we should move these web-only code into common code instead.
       * However, the temporary measure exists so that we can still have good type coverage during
       * experimentation before we can lock down the boundary. *)
      match additional with
      | Some _ -> additional
      | None ->
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
  match additional with
  | Some p' -> [p; p']
  | None -> [p]

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
