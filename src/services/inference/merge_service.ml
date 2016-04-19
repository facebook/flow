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

module LeaderHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
  type t = filename
  let prefix = Prefix.make()
end)

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
    let reason = Reason_js.mk_reason r loc in

    let m_name = Modulename.to_string resolved_r in
    let tvar = Flow_js.mk_tvar cx reason in
    Flow_js.lookup_builtin cx (Reason_js.internal_module_name m_name)
      reason (Some (Reason_js.builtin_reason m_name)) tvar

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

   (d) decls: edges between contexts in component_cxs and libraries, classified
   by requires (when implementations of such requires are not found).

   The arguments (b), (c), (d) are passed to `merge_component_strict`, and
   argument (a) is passed to `restore`.
*)
let merge_strict_context ~options cache component_cxs =
  let required = List.fold_left (fun required cx ->
    let require_locs = Context.require_loc cx in
    SSet.fold (fun r ->
      let loc =
        try SMap.find_unsafe r require_locs
        with Not_found -> raise (Key_not_found ("require_locs", r))
      in
      let resolved_r = Module_js.find_resolved_module ~options cx loc r in
      check_require (r, resolved_r, cx);
      add_decl (r, resolved_r, cx)
    ) (Context.required cx) required
  ) [] component_cxs in
  let cx = List.hd component_cxs in

  let sig_cache = new Context_cache.sig_context_cache in

  let orig_sig_cxs, sig_cxs, impls, decls =
    List.fold_left (fun (orig_sig_cxs, sig_cxs, impls, decls) req ->
      let r, resolved_r, cx_to = req in
      Module_js.(match get_module_file resolved_r with
      | Some file ->
          let info = get_module_info file in
          if info.checked && info.parsed then
            (* checked implementation exists *)
            let impl sig_cx = sig_cx, r, info._module, cx_to in
            begin match cache#find file with
            | Some sig_cx ->
                orig_sig_cxs, sig_cxs,
                (impl sig_cx) :: impls, decls
            | None ->
                let file =
                  try LeaderHeap.find_unsafe file
                  with Not_found ->
                    raise (Key_not_found ("LeaderHeap", (string_of_filename file)))
                in
                begin match sig_cache#find file with
                | Some sig_cx ->
                    orig_sig_cxs, sig_cxs,
                    (impl sig_cx) :: impls, decls
                | None ->
                    let orig_sig_cx, sig_cx = sig_cache#read file in
                    orig_sig_cx::orig_sig_cxs, sig_cx::sig_cxs,
                    (impl sig_cx) :: impls,
                    decls
                end
            end
          else
            (* unchecked implementation exists *)
            (* use required name as resolved name, for lib lookups *)
            let fake_resolved = Modulename.String r in
            orig_sig_cxs, sig_cxs, impls, (r, fake_resolved, cx_to) :: decls
      | None ->
          (* implementation doesn't exist *)
          orig_sig_cxs, sig_cxs, impls, (r, resolved_r, cx_to) :: decls
      )
    ) ([], [], [], []) required
  in

  let orig_master_cx, master_cx = sig_cache#read Loc.Builtins in

  Merge_js.merge_component_strict
    component_cxs sig_cxs impls decls master_cx;
  Merge_js.restore cx orig_sig_cxs orig_master_cx;

  ()

(* Entry point for merging a component *)
let merge_strict_component ~options (component: filename list) =
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
  let info = Module_js.get_module_info file in
  if info.Module_js.checked then (
    let cache = new Context_cache.context_cache in
    let component_cxs = List.map cache#read component in

    merge_strict_context ~options cache component_cxs;

    Merge_js.ContextOptimizer.sig_context component_cxs;
    let cx = List.hd component_cxs in
    let errors = Context.errors cx in
    Context.remove_all_errors cx;
    Context_cache.add_sig cx;
    file, errors
  )
  else file, Errors_js.ErrorSet.empty

let merge_strict_job ~options (merged, errsets) (components: filename list list) =
  List.fold_left (fun (merged, errsets) (component: filename list) ->
    let files = component
    |> List.map string_of_filename
    |> String.concat "\n\t"
    in
    try Profile_utils.checktime ~options 1.0
      (fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
      (fun () ->
        (*prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) file;*)
        let file, errors = merge_strict_component ~options component in
        file :: merged, errors :: errsets
      )
    with exc ->
      let file = List.hd component in
      let msg = "merge_strict_job exception: "^(fmt_exc exc) in
      let errorset = Errors_js.ErrorSet.singleton
        (Errors_js.internal_error file msg) in
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
      List.hd component :: merged, errorset::errsets
  ) (merged, errsets) components

let merge_strict ~options ~workers ~save_errors dependency_graph partition =
  (* NOTE: master_cx will only be saved once per server lifetime *)
  let master_cx = Init_js.get_master_cx options in
  (* TODO: we probably don't need to save master_cx in ContextHeap *)
  Context_cache.add master_cx;
  (* store master signature context to heap *)
  Context_cache.add_sig master_cx;
  (* make a map from component leaders to components *)
  let component_map =
    IMap.fold (fun _ components acc ->
      List.fold_left (fun acc component ->
        FilenameMap.add (List.hd component) component acc
      ) acc components
    ) partition FilenameMap.empty
  in
  (* make a map from files to their component leaders *)
  let leader_map =
    FilenameMap.fold (fun file component acc ->
      List.fold_left (fun acc file_ ->
        FilenameMap.add file_ file acc
      ) acc component
    ) component_map FilenameMap.empty
  in
  (* store leaders to a heap; used when rechecking *)
  leader_map |> FilenameMap.iter LeaderHeap.add;
  Profile_utils.logtime ~options
    (fun t -> spf "merged (strict) in %f" t)
    (fun () ->
      (* returns parallel lists of filenames and errorsets *)
      let (files, errsets) = MultiWorker.call_dynamic
        workers
        ~job: (merge_strict_job ~options)
        ~neutral: ([], [])
        ~merge: Merge_stream.join
        ~next: (Merge_stream.make dependency_graph leader_map component_map) in
      (* collect master context errors *)
      let (files, errsets) = (
        Context.file master_cx :: files,
        Context.errors master_cx :: errsets
      ) in
      (* save *)
      save_errors files errsets;
    )

let remove_batch to_merge =
  LeaderHeap.remove_batch to_merge;
  Context_cache.remove_sig_batch to_merge;
