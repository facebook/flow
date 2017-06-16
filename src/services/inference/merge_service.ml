(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(* To merge the contexts of a component (component_cxs) with their dependencies,
   we call the functions `merge_component_strict` and `restore` defined
   in type_inference_js.ml with appropriate arguments prepared below.

   First, we check the requires of component_cxs.

   Next, we traverse these requires, creating:

   (a) orig_sig_cxs: the original signature contexts of dependencies outside the
   component.

   (b) sig_cxs: the copied signature contexts of such dependencies.

   (c) impls: edges between contexts in component_cxs and sig_cxs that
   are labeled with the requires they denote (when implementations of such
   requires are found).

   (d) res: edges between contexts in component_cxs and resource files, labeled
   with the requires they denote.

   (e) decls: edges between contexts in component_cxs and libraries, classified
   by requires (when implementations of such requires are not found).

   The arguments (b), (c), (d) are passed to `merge_component_strict`, and
   argument (a) is passed to `restore`.
*)
let merge_strict_context_with_required ~options component_cxs required =
  let cx = List.hd component_cxs in

  let cache = List.fold_left (fun acc cx ->
    FilenameMap.add (Context.file cx) cx acc
  ) FilenameMap.empty component_cxs in

  let sig_cache = new Context_cache.sig_context_cache in

  let orig_dep_cxs, dep_cxs, impls, dep_impls, res, decls, unchecked =
    List.fold_left (fun (orig_dep_cxs, dep_cxs, impls, dep_impls, res, decls, unchecked) req ->
      let r, loc, resolved_r, cx_to = req in
      Module_js.(match get_file Expensive.ok resolved_r with
      | Some (Loc.ResourceFile f) ->
          orig_dep_cxs, dep_cxs,
          impls, dep_impls,
          (r, loc, f, cx_to) :: res, decls, unchecked
      | Some file ->
          let info = get_info_unsafe ~audit:Expensive.ok file in
          if info.checked && info.parsed then
            (* checked implementation exists *)
            match FilenameMap.get file cache with
            | Some cx ->
              (* impl is part of component *)
              orig_dep_cxs, dep_cxs,
              (cx, Files.module_ref file, r, cx_to) :: impls, dep_impls,
              res, decls, unchecked
            | None ->
              (* look up impl sig_context *)
              let impl cx = cx, Files.module_ref file, r, cx_to in
              let file = Context_cache.find_leader file in
              match sig_cache#find file with
              | Some sig_cx ->
                  orig_dep_cxs, dep_cxs,
                  impls, (impl sig_cx) :: dep_impls,
                  res, decls, unchecked
              | None ->
                  let orig_sig_cx, sig_cx =
                    sig_cache#read ~audit:Expensive.ok ~options file in
                  orig_sig_cx::orig_dep_cxs, sig_cx::dep_cxs,
                  impls, (impl sig_cx) :: dep_impls,
                  res, decls, unchecked
          else
            (* unchecked implementation exists *)
            orig_dep_cxs, dep_cxs,
            impls, dep_impls,
            res, decls, (r, loc, cx_to) :: unchecked
      | None ->
          (* implementation doesn't exist *)
          orig_dep_cxs, dep_cxs,
          impls, dep_impls,
          res, (r, loc, resolved_r, cx_to) :: decls, unchecked
      )
    ) ([], [], [], [], [], [], []) required
  in

  let orig_master_cx, master_cx =
    sig_cache#read ~audit:Expensive.ok ~options Loc.Builtins in

  Merge_js.merge_component_strict
    component_cxs impls
    dep_cxs dep_impls
    res decls unchecked master_cx;
  Merge_js.restore cx orig_dep_cxs orig_master_cx;

  orig_master_cx

let merge_strict_context ~options component_cxs =
  let required = List.fold_left (fun required cx ->
    let file = Context.file cx in
    let require_loc_map = Parsing_service_js.get_requires_unsafe file in
    SMap.fold (fun r loc ->
      let resolved_r = Module_js.find_resolved_module ~audit:Expensive.ok
        file r in
      List.cons (r, loc, resolved_r, cx)
    ) require_loc_map required
  ) [] component_cxs in
  merge_strict_context_with_required ~options component_cxs required

(* Variation of merge_strict_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let merge_contents_context ~options cx require_loc_map ~ensure_checked_dependencies =
  let resolved_rs, required =
    SMap.fold (fun r loc (resolved_rs, required) ->
      let resolved_r = Module_js.imported_module
        ~options
        ~node_modules_containers:!Files.node_modules_containers
        (Context.file cx) loc r in
      Module_js.NameSet.add resolved_r resolved_rs,
      (r, loc, resolved_r, cx) :: required
    ) require_loc_map (Module_js.NameSet.empty, [])
  in
  ensure_checked_dependencies resolved_rs;
  (merge_strict_context_with_required ~options [cx] required: Context.t)
  |> ignore

(* Entry point for merging a component *)
let merge_strict_component ~options component =
  let file = List.hd component in

  (* We choose file as the leader, and other_files are followers. It is always
     OK to choose file as leader, as explained below.

     Note that cycles cannot happen between unchecked files. Why? Because files
     in cycles must have their dependencies recorded, yet dependencies are never
     recorded for unchecked files.

     It follows that when file is unchecked, there are no other_files! We don't
     have to worry that some other_file may be checked when file is unchecked.

     It also follows when file is checked, other_files must be checked too!
  *)
  let info = Module_js.get_info_unsafe ~audit:Expensive.ok file in
  if info.Module_js.checked then (
    let component_cxs =
      List.map
        (Context_cache.get_context_unsafe ~audit:Expensive.ok ~options)
        component
    in

    let master_cx = merge_strict_context ~options component_cxs in

    let md5 = Merge_js.ContextOptimizer.sig_context component_cxs in
    let cx = List.hd component_cxs in

    Merge_js.clear_master_shared cx master_cx;

    let errors = Context.errors cx in

    Context.remove_all_errors cx;
    Context.clear_intermediates cx;

    let diff = Context_cache.add_merge_on_diff ~audit:Expensive.ok
      component_cxs md5 in
    file, errors, diff
  )
  else file, Errors.ErrorSet.empty, true

let merge_strict_job ~options (merged, unchanged) elements =
  List.fold_left (fun (merged, unchanged) -> function
    | Merge_stream.Skip file ->
      (* Skip rechecking this file (because none of its dependencies changed).
         We are going to reuse the existing signature for the file, so we must
         be careful that such a signature actually exists! This holds because a
         skipped file cannot be a direct dependency, so its dependencies cannot
         change, which in particular means that it belongs to the same component,
         although possibly with a different leader that has the signature. *)
      (* file was skipped, so its signature is definitely unchanged *)
      (merged, file::unchanged)
    | Merge_stream.Component component ->
      (* A component may have several files: there's always at least one, and
         multiple files indicate a cycle. *)
      let files = component
      |> List.map string_of_filename
      |> String.concat "\n\t"
      in
      try Profile_utils.checktime ~options ~limit:1.0
        ~msg:(fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
        ~log:(fun merge_time ->
          let length = List.length component in
          let leader = List.hd component |> string_of_filename in
          Flow_server_profile.merge ~length ~merge_time ~leader)
        ~f:(fun () ->
          (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
          let file, errors, diff = merge_strict_component ~options component in
          (* diff says whether its signature was changed *)
          (file, errors) :: merged,
          if diff then unchanged else file :: unchanged
      )
    with
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exc -> raise exc
    (* A catch all suppression is probably a bad idea... *)
    | exc ->
      let file = List.hd component in
      let loc = Loc.({ none with source = Some file }) in
      let msg = Flow_error.(EInternal (loc, MergeJobException exc)) in
      let error = Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file:file msg in
      let errorset = Errors.ErrorSet.singleton error in
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
      ((file, errorset) :: merged), unchanged
  ) (merged, unchanged) elements

(* make a map from component leaders to components *)
let merge_strict ~intermediate_result_callback ~options ~workers
    dependency_graph component_map recheck_map =
  (* make a map from files to their component leaders *)
  let leader_map =
    FilenameMap.fold (fun file component acc ->
      List.fold_left (fun acc file_ ->
        FilenameMap.add file_ file acc
      ) acc component
    ) component_map FilenameMap.empty
  in
  (* lift recheck map from files to leaders *)
  let recheck_leader_map = FilenameMap.map (
    List.exists (fun f -> FilenameMap.find_unsafe f recheck_map)
  ) component_map in
  Profile_utils.logtime ~options
    ~msg:(fun t -> spf "merged (strict) in %f" t)
    ~f:(fun () ->
      (* returns parallel lists of filenames and errorsets *)
      let merged, _ = MultiWorker.call
        workers
        ~job: (merge_strict_job ~options)
        ~neutral: ([], [])
        ~merge: (Merge_stream.join intermediate_result_callback)
        ~next: (Merge_stream.make
                  dependency_graph leader_map component_map recheck_leader_map) in
      merged
    )
