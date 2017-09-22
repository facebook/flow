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
module Reqs = Merge_js.Reqs


type 'a merge_results = (File_key.t * ('a, exn) result) list
type 'a merge_job =
  options:Options.t ->
  'a merge_results * File_key.t list ->
  File_key.t list ->
  'a merge_results * File_key.t list

(* To merge the contexts of a component with their dependencies, we call the
   functions `merge_component_strict` and `restore` defined in merge_js.ml
   with appropriate reqs prepared below.

   (a) orig_sig_cxs: the original signature contexts of dependencies outside the
   component.

   (b) sig_cxs: the copied signature contexts of such dependencies.

   (c) impls: edges between files within the component

   (d) dep_impls: edges from files in the component to cxs of direct
   dependencies, when implementations are found.

   (e) unchecked: edges from files in the component to files which are known to
   exist are not checked (no @flow, @noflow, unparsed). Note that these
   dependencies might be provided by a (typed) libdef, but we don't know yet.

   (f) res: edges between files in the component and resource files, labeled
   with the requires they denote.

   (g) decls: edges between files in the component and libraries, classified
   by requires (when implementations of such requires are not found).
*)
let reqs_of_component ~options component required =
  let dep_cxs, reqs =
    List.fold_left (fun (dep_cxs, reqs) req ->
      let r, loc, resolved_r, file = req in
      Module_js.(match get_file Expensive.ok resolved_r with
      | Some (File_key.ResourceFile f) ->
        dep_cxs, Reqs.add_res (r, loc, f, file) reqs
      | Some dep ->
        let info = get_info_unsafe ~audit:Expensive.ok dep in
        if info.checked && info.parsed then
          (* checked implementation exists *)
          let m = Files.module_ref dep in
          if List.mem dep component then
            (* impl is part of component *)
            dep_cxs, Reqs.add_impl (dep, m, r, file) reqs
          else
            (* look up impl sig_context *)
            let leader = Context_cache.find_leader dep in
            let dep_cx = Context_cache.find_sig ~options leader in
            dep_cx::dep_cxs, Reqs.add_dep_impl (dep_cx, m, r, file) reqs
        else
          (* unchecked implementation exists *)
          dep_cxs, Reqs.add_unchecked (r, loc, file) reqs
      | None ->
        (* implementation doesn't exist *)
        dep_cxs, Reqs.add_decl (r, loc, resolved_r, file) reqs
      )
    ) ([], Reqs.empty) required
  in

  let master_cx = Context_cache.find_sig ~options File_key.Builtins in

  master_cx, dep_cxs, reqs

let merge_strict_context ~options component =
  let required, file_sigs =
    List.fold_left (fun (required, file_sigs) file ->
      let file_sig = Parsing_service_js.get_file_sig_unsafe file in
      let require_loc_map = File_sig.(require_loc_map file_sig.module_sig) in
      let required = SMap.fold (fun r loc ->
        let resolved_r = Module_js.find_resolved_module ~audit:Expensive.ok
          file r in
        List.cons (r, loc, resolved_r, file)
      ) require_loc_map required in
      required, FilenameMap.add file file_sig file_sigs
    ) ([], FilenameMap.empty) component in

  let master_cx, dep_cxs, file_reqs =
    reqs_of_component ~options component required
  in

  let metadata = Context.metadata_of_options options in
  let lint_severities = Some (Options.lint_severities options) in
  let cx = Merge_js.merge_component_strict
    ~metadata ~lint_severities ~file_sigs
    ~get_ast_unsafe:Parsing_service_js.get_ast_unsafe
    ~get_docblock_unsafe:Parsing_service_js.get_docblock_unsafe
    component file_reqs dep_cxs master_cx
  in

  cx, master_cx

(* Variation of merge_strict_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let merge_contents_context options file ast info ~ensure_checked_dependencies =
  let file_sig = Parsing_service_js.calc_file_sig ~ast in
  let resolved_rs, required =
    let require_loc_map = File_sig.(require_loc_map file_sig.module_sig) in
    SMap.fold (fun r loc (resolved_rs, required) ->
      let resolved_r = Module_js.imported_module
        ~options
        ~node_modules_containers:!Files.node_modules_containers
        file loc r in
      Modulename.Set.add resolved_r resolved_rs,
      (r, loc, resolved_r, file) :: required
    ) require_loc_map (Modulename.Set.empty, [])
  in
  let file_sigs = FilenameMap.singleton file file_sig in

  ensure_checked_dependencies resolved_rs;

  let component = [file] in

  let master_cx, dep_cxs, file_reqs =
    begin try reqs_of_component ~options component required with
      | Key_not_found _  ->
        failwith "not all dependencies are ready yet, aborting..."
      | e -> raise e
    end
  in

  let metadata = Context.metadata_of_options options in
  let lint_severities = Some (Options.lint_severities options) in
  let cx = Merge_js.merge_component_strict
    ~metadata ~lint_severities ~file_sigs
    ~get_ast_unsafe:(fun _ -> ast)
    ~get_docblock_unsafe:(fun _ -> info)
    component file_reqs dep_cxs master_cx
  in

  cx

(* Entry point for merging a component *)
let merge_strict_component ~options (merged_acc, unchanged_acc) component =
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
    let cx, master_cx = merge_strict_context ~options component in

    let module_refs = List.rev_map Files.module_ref component in
    let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in

    Merge_js.clear_master_shared cx master_cx;

    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
    let severity_cover = Context.severity_cover cx in

    Context.remove_all_errors cx;
    Context.remove_all_error_suppressions cx;
    Context.remove_all_lint_severities cx;

    Context.clear_intermediates cx;

    let diff = Context_cache.add_merge_on_diff ~audit:Expensive.ok
      cx component md5 in

    (file, Ok (errors, suppressions, severity_cover)) :: merged_acc,
    if diff then unchanged_acc else file :: unchanged_acc
  )
  else
    let errors = Errors.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let severity_cover =
      ExactCover.file_cover file
        (Options.lint_severities options)
    in
    (file, Ok (errors, suppressions, severity_cover)) :: merged_acc,
    unchanged_acc

let merge_strict_job ~options ~job (merged, unchanged) elements =
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
      |> List.map File_key.to_string
      |> String.concat "\n\t"
      in
      try Profile_utils.checktime ~options ~limit:1.0
        ~msg:(fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
        ~log:(fun merge_time ->
          let length = List.length component in
          let leader = List.hd component |> File_key.to_string in
          Flow_server_profile.merge ~length ~merge_time ~leader)
        ~f:(fun () ->
          (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
          job ~options (merged, unchanged) component
        )
      with
      | SharedMem_js.Out_of_shared_memory
      | SharedMem_js.Heap_full
      | SharedMem_js.Hash_table_full
      | SharedMem_js.Dep_table_full as exc -> raise exc
      (* A catch all suppression is probably a bad idea... *)
      | exc when not Build_mode.dev ->
        let file = List.hd component in
        prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
          (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
        ((file, Error exc) :: merged), unchanged
  ) (merged, unchanged) elements

(* make a map from component leaders to components *)
let merge_runner ~job ~intermediate_result_callback ~options ~workers
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
      (* returns parallel lists of filenames, error sets, and suppression sets *)
      let merged, _ = MultiWorker.call
        workers
        ~job: (merge_strict_job ~options ~job)
        ~neutral: ([], [])
        ~merge: (Merge_stream.join intermediate_result_callback)
        ~next: (Merge_stream.make
                  dependency_graph leader_map component_map recheck_leader_map) in
      merged
    )

let merge_strict = merge_runner ~job:merge_strict_component
