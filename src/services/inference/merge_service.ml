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

(* Warn on a missing required module resolved_r referenced as r in context cx.

   TODO maybe make this suppressable
*)
let check_require (r, resolved_r, cx) =
  if not (Module_js.module_exists resolved_r)
  then
    let loc =
      try SMap.find_unsafe r (Context.require_loc cx)
      with Not_found -> raise (Key_not_found ("Context.require_loc", r))
    in
    let reason = Reason.mk_reason (Reason.RCustom r) loc in

    let m_name = Modulename.to_string resolved_r in
    let tvar = Flow_js.mk_tvar cx reason in
    Flow_js.lookup_builtin cx (Reason.internal_module_name m_name)
      reason (Type.Strict (Reason.builtin_reason (Reason.RCustom m_name))) tvar

let add_decl (r, resolved_r, cx) declarations =
  (r, resolved_r, cx) :: declarations

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
let merge_strict_context_with_required cache component_cxs required =
  let cx = List.hd component_cxs in

  let sig_cache = new Context_cache.sig_context_cache in

  let orig_sig_cxs, sig_cxs, impls, res, decls =
    List.fold_left (fun (orig_sig_cxs, sig_cxs, impls, res, decls) req ->
      let r, resolved_r, cx_to = req in
      Module_js.(match get_file Expensive.ok resolved_r with
      | Some (Loc.ResourceFile f) ->
          orig_sig_cxs, sig_cxs,
          impls, (r, f, cx_to) :: res, decls
      | Some file ->
          let info = get_info_unsafe ~audit:Expensive.ok file in
          if info.checked && info.parsed then
            (* checked implementation exists *)
            let impl sig_cx = sig_cx, Files.module_ref file, r, cx_to in
            begin match cache#find file with
            | Some sig_cx ->
                orig_sig_cxs, sig_cxs,
                (impl sig_cx) :: impls, res, decls
            | None ->
                let file = Context_cache.find_leader file in
                begin match sig_cache#find file with
                | Some sig_cx ->
                    orig_sig_cxs, sig_cxs,
                    (impl sig_cx) :: impls, res, decls
                | None ->
                    let orig_sig_cx, sig_cx =
                      sig_cache#read ~audit:Expensive.ok file in
                    orig_sig_cx::orig_sig_cxs, sig_cx::sig_cxs,
                    (impl sig_cx) :: impls, res, decls
                end
            end
          else
            (* unchecked implementation exists *)
            (* use required name as resolved name, for lib lookups *)
            let fake_resolved = Modulename.String r in
            orig_sig_cxs, sig_cxs,
            impls, res, (r, fake_resolved, cx_to) :: decls
      | None ->
          (* implementation doesn't exist *)
          orig_sig_cxs, sig_cxs,
          impls, res, (r, resolved_r, cx_to) :: decls
      )
    ) ([], [], [], [], []) required
  in

  let orig_master_cx, master_cx =
    sig_cache#read ~audit:Expensive.ok Loc.Builtins in

  Merge_js.merge_component_strict
    component_cxs sig_cxs impls res decls master_cx;
  Merge_js.restore cx orig_sig_cxs orig_master_cx;

  orig_master_cx

let merge_strict_context cache component_cxs =
  let required = List.fold_left (fun required cx ->
    SSet.fold (fun r ->
      let resolved_r = Module_js.find_resolved_module ~audit:Expensive.ok
        (Context.file cx) r in
      check_require (r, resolved_r, cx);
      add_decl (r, resolved_r, cx)
    ) (Context.required cx) required
  ) [] component_cxs in
  merge_strict_context_with_required cache component_cxs required

(* Variation of merge_strict_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let merge_contents_context ~options cache cx =
  let required =
    let require_locs = Context.require_loc cx in
    SSet.fold (fun r ->
      let loc =
        try SMap.find_unsafe r require_locs
        with Not_found -> raise (Key_not_found ("require_locs", r))
      in
      let resolved_r = Module_js.imported_module
        ~options
        ~node_modules_containers:!Files.node_modules_containers
        (Context.file cx) loc r in
      check_require (r, resolved_r, cx);
      add_decl (r, resolved_r, cx)
    ) (Context.required cx) []
  in
  (merge_strict_context_with_required cache [cx] required: Context.t)
  |> ignore

(* Entry point for merging a component *)
let merge_strict_component = function
  | Merge_stream.Skip file ->
    (* Skip rechecking this file (because none of its dependencies changed). We
       are going to reuse the existing signature for the file, so we must be
       careful that such a signature actually exists! This holds because a
       skipped file cannot be a direct dependency, so its dependencies cannot
       change, which in particular means that it belongs to the same component,
       although possibly with a different leader that has the signature. *)
    file, Errors.ErrorSet.empty, None
  | Merge_stream.Component component ->
  (* A component may have several files: there's always at least one, and
     multiple files indicate a cycle. *)

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
    let cache = new Context_cache.context_cache in
    let component_cxs =
      List.map (cache#read ~audit:Expensive.ok) component in

    let master_cx = merge_strict_context cache component_cxs in

    let md5 = Merge_js.ContextOptimizer.sig_context component_cxs in
    let cx = List.hd component_cxs in

    Merge_js.clear_master_shared cx master_cx;

    let errors = Context.errors cx in

    Context.remove_all_errors cx;
    Context.clear_intermediates cx;

    let diff = Context_cache.add_merge_on_diff ~audit:Expensive.ok
      component_cxs md5 in
    file, errors, Some diff
  )
  else file, Errors.ErrorSet.empty, Some true

let merge_strict_job ~options (merged, unchanged) elements =
  List.fold_left (fun (merged, unchanged) element ->
    let component = Merge_stream.(match element with
      | Component component -> component
      | Skip file -> [file]
    ) in
    let files = component
    |> List.map string_of_filename
    |> String.concat "\n\t"
    in
    try Profile_utils.checktime ~options 1.0
      (fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
      (fun () ->
        (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
        let file, errors, diff = merge_strict_component element in
        match diff with
        | Some diff ->
          (* file was rechecked; diff says whether its signature was changed *)
          (file, errors) :: merged,
          if diff then unchanged else file :: unchanged
        | None ->
          (* file was skipped, so its signature is definitely unchanged *)
          merged,
          file :: unchanged
      )
    with
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exc -> raise exc
    (* A catch all suppression is probably a bad idea... *)
    | exc ->
      let file = List.hd component in
      let msg = "merge_strict_job exception: "^(fmt_exc exc) in
      let errorset = Errors.ErrorSet.singleton
        (Errors.internal_error file msg) in
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
      (file, errorset) :: merged, unchanged
  ) (merged, unchanged) elements

(* make a map from component leaders to components *)
let merge_strict ~options ~workers
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
    (fun t -> spf "merged (strict) in %f" t)
    (fun () ->
      (* returns parallel lists of filenames and errorsets *)
      let merged, _ = MultiWorker.call
        workers
        ~job: (merge_strict_job ~options)
        ~neutral: ([], [])
        ~merge: Merge_stream.join
        ~next: (Merge_stream.make
                  dependency_graph leader_map component_map recheck_leader_map) in
      merged
    )
