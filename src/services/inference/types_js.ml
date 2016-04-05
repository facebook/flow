(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module drives the type checker *)

open Utils_js

module TI = Type_inference_js
module Errors = Errors_js

exception Key_not_found of (* message *) string * (* key *) string

let is_lib_file f = match f with
| Loc.LibFile _ -> true
| _ -> false

(****************** shared context heap *********************)

(* map from file names to contexts *)
(* NOTE: Entries are cached for performance, since contexts are read a lot more
   than they are written. But this means that proper care must be taken when
   reading contexts: in particular, context graphs have mutable bounds, so they
   must be copied, otherwise bad things will happen. (The cost of copying
   context graphs is presumably a lot less than deserializing contexts, so the
   optimization makes sense. *)
module ContextHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
  type t = Context.t
  let prefix = Prefix.make()
end)

module SigContextHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
  type t = Context.t
  let prefix = Prefix.make()
end)

(* errors are stored by phase,
   in maps from file path to error set
 *)
(* errors encountered during parsing *)
let parse_errors = ref FilenameMap.empty
(* errors encountered during local inference *)
let infer_errors = ref FilenameMap.empty
(* errors encountered during module commit *)
let module_errors = ref FilenameMap.empty
(* errors encountered during merge *)
let merge_errors = ref FilenameMap.empty
(* error suppressions in the code *)
let error_suppressions = ref FilenameMap.empty
(* aggregate error map, built after check or recheck
   by collate_errors
 *)
let all_errors = ref FilenameMap.empty

(****************** typecheck job helpers *********************)

(* error state handling.
   note: once weve decoupled from hack binary, these will be stored
   in the recurrent env struct, not local state *)
let clear_errors ?(debug=false) (files: filename list) =
  List.iter (fun file ->
    if debug then prerr_endlinef "clear errors %s" (string_of_filename file);
    parse_errors := FilenameMap.remove file !parse_errors;
    infer_errors := FilenameMap.remove file !infer_errors;
    module_errors := FilenameMap.remove file !module_errors;
    merge_errors := FilenameMap.remove file !merge_errors;
    error_suppressions := FilenameMap.remove file !error_suppressions;
  ) files;
  all_errors := FilenameMap.empty

(* helper - save an error set into a global error map.
   clear mapping if errorset is empty *)
let save_errset mapref file errset =
  mapref := if Errors.ErrorSet.cardinal errset = 0
    then FilenameMap.remove file !mapref
    else FilenameMap.add file errset !mapref

(* given a reference to a error map (files to errorsets), and
   two parallel lists of such, save the latter into the former. *)
let save_errors mapref files errsets =
  List.iter2 (save_errset mapref) files errsets

let save_errormap mapref errmap =
  FilenameMap.iter (save_errset mapref) errmap

(* given a reference to a error suppression map (files to
   error suppressions), and two parallel lists of such,
   save the latter into the former. *)
let save_suppressions mapref files errsups = Errors.(
  List.iter2 (fun file errsup ->
    mapref := if ErrorSuppressions.cardinal errsup = 0
      then FilenameMap.remove file !mapref
      else FilenameMap.add file errsup !mapref
  ) files errsups
)

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs = Errors.(

  let distrib_error _orig_file error error_map =
    let file = match Loc.source (loc_of_error error) with
    | Some x -> x
    | None ->
        assert_false (spf "distrib_errs: no source for error: %s"
          (Hh_json.json_to_multiline (json_of_errors [error])));
    in
    match FilenameMap.get file error_map with
    | None ->
      FilenameMap.add file (ErrorSet.singleton error) error_map
    | Some error_set ->
      if ErrorSet.mem error error_set
      then error_map
      else FilenameMap.add file (ErrorSet.add error error_set) error_map
  in

  fun _file file_errors error_map ->
    ErrorSet.fold (distrib_error _file) file_errors error_map
)
(* Given all the errors as a map from file => errorset
 * 1) Filter out the suppressed errors from the error sets
 * 2) Remove files with empty errorsets from the map
 * 3) Add errors for unused suppressions
 * 4) Properly distribute the new errors
 *)
let filter_suppressed_errors emap = Errors.(
  let suppressions = ref ErrorSuppressions.empty in

  let filter_suppressed_error error =
    let (suppressed, sups) = ErrorSuppressions.check error !suppressions in
    suppressions := sups;
    not suppressed
  in

  suppressions := FilenameMap.fold
    (fun _key -> ErrorSuppressions.union)
    !error_suppressions
    ErrorSuppressions.empty;

  let emap = emap
    |> FilenameMap.map (ErrorSet.filter filter_suppressed_error)
    |> FilenameMap.filter (fun _ v -> not (ErrorSet.is_empty v)) in

  (* For each unused suppression, create an error *)
  let unused_suppression_errors = List.fold_left
    (fun errset loc ->
      let err = Errors.mk_error [
        loc, ["Error suppressing comment"; "Unused suppression"]
      ] in
      ErrorSet.add err errset
    )
    ErrorSet.empty
    (ErrorSuppressions.unused !suppressions) in

    distrib_errs "" unused_suppression_errors emap
)

(* retrieve a full error list.
   Library errors are forced to the top of the list.
   Note: in-place conversion using an array is to avoid
   memory pressure on pathologically huge error sets, but
   this may no longer be necessary
 *)
let get_errors () =
  let all = !all_errors in
  let all = filter_suppressed_errors all in

  (* flatten to list, with lib errors bumped to top *)
  let append_errset errset errlist =
    List.rev_append (Errors.ErrorSet.elements errset) errlist
  in
  let revlist_libs, revlist_others = FilenameMap.fold (
    fun file errset (libs, others) ->
      if is_lib_file file
      then append_errset errset libs, others
      else libs, append_errset errset others
    ) all ([], [])
  in
  List.rev_append revlist_libs (List.rev revlist_others)

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors () =
  all_errors :=
    !parse_errors
    |> FilenameMap.fold distrib_errs !infer_errors
    |> FilenameMap.fold distrib_errs !merge_errors
    |> FilenameMap.fold distrib_errs !module_errors

let profile_and_not_quiet opts =
  Options.(opts.opt_profile && not opts.opt_quiet)

let wraptime opts pred msg f =
  if profile_and_not_quiet opts then time pred msg f
  else f()

let checktime opts limit msg f =
  wraptime opts (fun t -> t > limit) msg f

let logtime opts msg f =
  wraptime opts (fun _ -> true) msg f

let internal_error filename msg =
  let position = Loc.({
    line = 1;
    column = 0;
    offset = 0;
  }) in
  let loc = Loc.({
    source = Some filename;
    start = position;
    _end = position;
  }) in
  Errors.(simple_error ~kind:InternalError loc msg)

let apply_docblock_overrides metadata docblock_info =
  let open Context in

  let metadata = match Docblock.flow docblock_info with
  | None -> metadata
  | Some Docblock.OptIn -> { metadata with checked = true; }
  | Some Docblock.OptInWeak -> { metadata with checked = true; weak = true }

  (* --all (which sets metadata.checked = true) overrides @noflow, so there are
     currently no scenarios where we'd change checked = true to false. in the
     future, there may be a case where checked defaults to true (but is not
     forced to be true ala --all), but for now we do *not* want to force
     checked = false here. *)
  | Some Docblock.OptOut -> metadata
  in

  match Docblock.preventMunge docblock_info with
  | Some value -> { metadata with munge_underscores = not value; }
  | None -> metadata

(* Given a filename, retrieve the parsed AST, derive a module name,
   and invoke the local (infer) pass. This will build and return a
   fresh context object for the module. *)
let infer_module ~options ~metadata filename =
  let ast, info = Parsing_service_js.get_ast_and_info_unsafe filename in
  let module_name = Module_js.exported_module ~options filename info in
  let metadata = apply_docblock_overrides metadata info in
  TI.infer_ast ~metadata ~filename ~module_name ast

(* local inference job:
   takes list of filenames, accumulates into parallel lists of
   filenames, error sets *)
let infer_job opts (inferred, errsets, errsuppressions) files =
  let metadata = Context.metadata_of_options opts in
  List.fold_left (fun (inferred, errsets, errsuppressions) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: inferred %s in %f" (string_of_filename file) t)
      (fun () ->
        (*prerr_endlinef "[%d] INFER: %s" (Unix.getpid()) file;*)

        (* infer produces a context for this module *)
        let cx = infer_module ~options:opts ~metadata file in
        (* register module info *)
        Module_js.add_module_info ~options:opts cx;
        (* note: save and clear errors and error suppressions before storing
         * cx to shared heap *)
        let errs = Context.errors cx in
        let suppressions = Context.error_suppressions cx in
        Context.remove_all_errors cx;
        Context.remove_all_error_suppressions cx;

        let cx_file = Context.file cx in
        ContextHeap.add cx_file cx;

        (* add filename, errorset, suppressions *)
        cx_file :: inferred, errs :: errsets, suppressions :: errsuppressions
      )
    with exc ->
      let errorset = Errors.ErrorSet.singleton
        (internal_error file ("infer_job exception: "^(fmt_exc exc))) in
      prerr_endlinef "(%d) infer_job THROWS: %s"
        (Unix.getpid()) (fmt_file_exc (string_of_filename file) exc);
      file::inferred,
        errorset::errsets,
        Errors.ErrorSuppressions.empty::errsuppressions
  ) (inferred, errsets, errsuppressions) files

let rev_append_pair (x1, y1) (x2, y2) =
  (List.rev_append x1 x2, List.rev_append y1 y2)

let rev_append_triple (x1, y1, z1) (x2, y2, z2) =
  (List.rev_append x1 x2, List.rev_append y1 y2, List.rev_append z1 z2)

(* local type inference pass.
   Returns a set of successfully inferred files.
   Creates contexts for inferred files, with errors in cx.errors *)
let infer workers files opts =
  let files = FilenameSet.elements files in
  logtime opts
    (fun t -> spf "inferred %d files in %f" (List.length files) t)
    (fun () ->
      let files, errors, suppressions = MultiWorker.call
        workers
        ~job: (infer_job opts)
        ~neutral: ([], [], [])
        ~merge: rev_append_triple
        ~next: (Bucket.make files) in
      save_errors infer_errors files errors;
      save_suppressions error_suppressions files suppressions;
      files
    )

let checked m = Module_js.(
  let info = m |> get_file |> get_module_info in
  info.checked
)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file r = Module_js.(
  if module_exists r && checked r
  then Some (get_file r)
  else None
)

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

(* We already cache contexts in the shared memory for performance, but context
   graphs need to be copied because they have mutable bounds. We maintain an
   additional cache of local copies. Mutating bounds in local copies of context
   graphs is not only OK, but we rely on it during merging, so it is both safe
   and necessary to cache the local copies. As a side effect, this probably
   helps performance too by avoiding redundant copying. *)
class context_cache = object
  val cached_infer_contexts = Hashtbl.create 0

  (* find a context in the cache *)
  method find file =
    try Some (Hashtbl.find cached_infer_contexts file)
    with _ -> None

  (* read a context from shared memory, copy its graph, and cache the context *)
  method read file =
    let orig_cx =
      try ContextHeap.find_unsafe file
      with Not_found ->
        raise (Key_not_found ("ContextHeap", (string_of_filename file)))
    in
    let cx = Context.copy_of_context orig_cx in
    Hashtbl.add cached_infer_contexts file cx;
    cx
end

(* Similar to above, but for "signature contexts." The only differences are that
   the underlying heap is SigContextHeap instead of ContextHeap, and that `read`
   returns both the original and the copied version of a context. *)
class sig_context_cache = object
  val cached_merge_contexts = Hashtbl.create 0

  (* find a context in the cache *)
  method find file =
    try Some (Hashtbl.find cached_merge_contexts file)
    with _ -> None

  (* read a context from shared memory, copy its graph, and cache the context *)
  method read file =
    let orig_cx =
      try SigContextHeap.find_unsafe file
      with Not_found ->
        raise (Key_not_found ("SigContextHeap", (string_of_filename file)))
    in
    let cx = Context.copy_of_context orig_cx in
    Hashtbl.add cached_merge_contexts file cx;
    orig_cx, cx
end

let add_decl (r, resolved_r, cx) declarations =
  (r, resolved_r, cx) :: declarations

(**********************************)
(* entry point for merging a file *)
(**********************************)
module LeaderHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
  type t = filename
  let prefix = Prefix.make()
end)

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

  let sig_cache = new sig_context_cache in

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
    let cache = new context_cache in
    let component_cxs = List.map cache#read component in

    merge_strict_context ~options cache component_cxs;

    Merge_js.ContextOptimizer.sig_context component_cxs;
    let cx = List.hd component_cxs in
    let errors = Context.errors cx in
    Context.remove_all_errors cx;
    SigContextHeap.add file cx;
    file, errors
  )
  else file, Errors.ErrorSet.empty

let with_timer ?opts timer timing f =
  let timing = FlowEventLogger.Timing.start_timer ~timer timing in
  let ret = f () in
  let timing = FlowEventLogger.Timing.stop_timer ~timer timing in

  (* If we're profiling then output timing information to stderr *)
  (match opts with
  | Some options when profile_and_not_quiet options ->
      (match FlowEventLogger.Timing.get_finished_timer ~timer timing with
      | Some (start_wall_age, wall_duration) ->
          prerr_endlinef
            "TimingEvent `%s`: start_wall_age: %f; wall_duration: %f"
            timer
            start_wall_age
            wall_duration
      | _ -> ());
  | _ -> ());

  (timing, ret)

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents ~options ?verbose contents filename =
  let timing = FlowEventLogger.Timing.create () in

  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let use_strict = Options.modules_are_use_strict options in
  let timing, (parse_result, info) = with_timer "Parsing" timing (fun () ->
    let info = Docblock.extract (string_of_filename filename) contents in
    let parse_result = Parsing_service_js.do_parse
      ~fail:false ~types_mode ~use_strict ~info
      contents filename
    in
    parse_result, info
  ) in

  match parse_result with
  | Parsing_service_js.Parse_ok ast ->
      (* defaults *)
      let metadata = { (Context.metadata_of_options options) with
        Context.checked = true;
        Context.verbose = verbose;
      } in
      (* apply overrides from the docblock *)
      let metadata = apply_docblock_overrides metadata info in

      let timing, cx = with_timer "Infer" timing (fun () ->
        TI.infer_ast
          ~gc:false ~metadata ~filename ~module_name:(Modulename.String "-") ast
      ) in

      let cache = new context_cache in
      let timing, () = with_timer "Merge" timing (fun () ->
        merge_strict_context ~options cache [cx]
      ) in

      (* Filter out suppressed errors *)
      let error_suppressions = Context.error_suppressions cx in
      let errors = (Context.errors cx) |> Errors.ErrorSet.filter (fun err ->
        not (fst (Errors.ErrorSuppressions.check err error_suppressions))
      ) in

      timing, Some cx, errors, info

  | Parsing_service_js.Parse_err errors ->
      timing, None, errors, info

  | Parsing_service_js.Parse_skip ->
      (* should never happen *)
      timing, None, Errors.ErrorSet.empty, info

let merge_strict_job opts (merged, errsets) (components: filename list list) =
  List.fold_left (fun (merged, errsets) (component: filename list) ->
    let files = component
    |> List.map string_of_filename
    |> String.concat "\n\t"
    in
    try checktime opts 1.0
      (fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
      (fun () ->
        (*prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) file;*)
        let file, errors = merge_strict_component ~options:opts component in
        file :: merged, errors :: errsets
      )
    with exc ->
      let file = List.hd component in
      let errorset = Errors.ErrorSet.singleton
        (internal_error file ("merge_strict_job exception: "^(fmt_exc exc))) in
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
      List.hd component :: merged, errorset::errsets
  ) (merged, errsets) components

(* Custom bucketing scheme for dynamically growing and shrinking workloads when
   merging files.

   We start out with files that have no dependencies: these files are available
   for scheduling merge jobs. All other files are "blocked", i.e., they are *not
   ready* for scheduling.

   NOTE: Scheduling merge jobs too early will cause crashes, since they will
   need stuff that has not been computed yet! A more sophisticated scheme may be
   designed to be tolerant to such failures, but the merge process is
   complicated enough as is. Also, performance-wise blocking does not seem to be
   an issue because files get unblocked pretty regularly (see below).

   Each blocked file maintains a counter on the number of files blocking
   them. As files are done, they decrement the counters of other files blocked
   on them. As soon as some of the counters go to zero, the corresponding files
   are made available for scheduling.

   Finally, we maintain a counter on the total number of blocked files. When
   that goes to zero, we prepare to exit!

   The underlying worker management scheme needs to know when to wait for more
   work vs. when it can safely exit. We signal the former by returning a `None`
   bucket, and the latter by returning a `Some []` bucket.
*)
module MergeStream = struct
  let max_bucket_size = 500 (* hard-coded, as in Bucket *)

  (* For each leader, maps the number of leaders it is currently blocking on. *)
  let blocking = Hashtbl.create 0
  (* Counts the number of blocked leaders. *)
  let blocked = ref 0

  (* For each leader, maps other leaders that are dependent on it. *)
  let dependents = ref FilenameMap.empty

  (* stream of files available to schedule *)
  let stream = ref []

  (* take n files from stream *)
  let rec take n =
    if n = 0 then []
    else match !stream with
    | [] -> assert false
    | x::rest ->
        stream := rest;
        x::(take (n-1))

  (* leader_map is a map from files to leaders *)
  (* component_map is a map from leaders to components *)
  (* dependency_graph is a map from files to dependencies *)
  let make dependency_graph leader_map component_map =
    (* TODO: clear or replace state *)
    let procs = Sys_utils.nbr_procs in
    let leader f = FilenameMap.find_unsafe f leader_map in
    let component f = FilenameMap.find_unsafe f component_map in

    let dependency_dag = FilenameMap.fold (fun f fs dependency_dag ->
      let leader_f = leader f in
      let dep_leader_fs = match FilenameMap.get leader_f dependency_dag with
        | Some dep_leader_fs -> dep_leader_fs
        | _ -> FilenameSet.empty
      in
      let dep_leader_fs = FilenameSet.fold (fun f dep_leader_fs ->
        let f = leader f in
        if f = leader_f then dep_leader_fs
        else FilenameSet.add f dep_leader_fs
      ) fs dep_leader_fs in
      FilenameMap.add leader_f dep_leader_fs dependency_dag
    ) dependency_graph FilenameMap.empty in

    FilenameMap.iter (fun leader_f dep_leader_fs ->
      let n = FilenameSet.cardinal dep_leader_fs in
      (* n files block leader_f *)
      Hashtbl.add blocking leader_f n;
      if n = 0
      then (* leader_f isn't blocked, add to stream *)
        stream := leader_f::!stream
      else (* one more blocked *)
        incr blocked
    ) dependency_dag;

    (* TODO: remember reverse dependencies to quickly calculate remerge sets *)
    dependents := Sort_js.reverse dependency_dag;

    fun () ->
      let jobs = List.length !stream in
      if jobs = 0 && !blocked <> 0 then MultiWorker.Wait
      else
        let bucket_size =
          if jobs < procs * max_bucket_size
          then 1 + (jobs / procs)
          else max_bucket_size
        in
        let n = min bucket_size jobs in
        let result = take n |> List.map component in
        MultiWorker.Job result

  (* We know when files are done by having jobs return the files they processed,
     and trapping the function that joins results. ;), yeah. *)
  let join =
    let push (leader_fs: filename list) =
      List.iter (fun leader_f ->
        FilenameSet.iter (fun dep_leader_f ->
          let n = (Hashtbl.find blocking dep_leader_f) - 1 in
          (* dep_leader blocked on one less *)
          Hashtbl.replace blocking dep_leader_f n;
          if n = 0 then
            (* one less blocked; add dep_leader_f to stream *)
            (decr blocked; stream := dep_leader_f::!stream)
        ) (FilenameMap.find_unsafe leader_f !dependents)
      ) leader_fs
    in
    fun res acc ->
      let leader_fs, _ = res in
      push leader_fs;
      rev_append_pair res acc

end

let merge_strict workers dependency_graph partition opts =
  (* NOTE: master_cx will only be saved once per server lifetime *)
  let master_cx = Init_js.get_master_cx opts in
  (* TODO: we probably don't need to save master_cx in ContextHeap *)
  ContextHeap.add Loc.Builtins master_cx;
  (* store master signature context to heap *)
  SigContextHeap.add Loc.Builtins master_cx;
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
  logtime opts
    (fun t -> spf "merged (strict) in %f" t)
    (fun () ->
      (* returns parallel lists of filenames and errorsets *)
      let (files, errsets) = MultiWorker.call_dynamic
        workers
        ~job: (merge_strict_job opts)
        ~neutral: ([], [])
        ~merge: MergeStream.join
        ~next: (MergeStream.make dependency_graph leader_map component_map) in
      (* collect master context errors *)
      let (files, errsets) = (
        Context.file master_cx :: files,
        Context.errors master_cx :: errsets
      ) in
      (* save *)
      save_errors merge_errors files errsets;
    )

(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)

let calc_dependencies_job =
  List.fold_left (fun deps file ->
    let { Module_js.required; _ } = Module_js.get_module_info file in
    let files = Module_js.NameSet.fold (fun r files ->
      match implementation_file r with
      | Some f -> FilenameSet.add f files
      | None -> files
    ) required FilenameSet.empty in
    FilenameMap.add file files deps
  )

let calc_dependencies workers files =
  let deps = MultiWorker.call
    workers
    ~job: calc_dependencies_job
    ~neutral: FilenameMap.empty
    ~merge: FilenameMap.union
    ~next: (Bucket.make files) in
  deps |> FilenameMap.map (
    FilenameSet.filter (fun f -> FilenameMap.mem f deps))

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules workers ~options inferred removed =
  let errmap = Module_js.commit_modules workers ~options inferred removed in
  save_errormap module_errors errmap

(* Sanity checks on InfoHeap and NameHeap. Since this is performance-intensive
   (although it probably doesn't need to be), it is only done under --debug. *)
let heap_check files = Module_js.(
  let ih = Hashtbl.create 0 in
  let nh = Hashtbl.create 0 in
  files |> List.iter (fun file ->
    let m_file = get_file (Modulename.Filename file) in
    if not (Loc.check_suffix m_file Files_js.flow_ext)
    then assert (m_file = file);
    let info = get_module_info file in
    Hashtbl.add ih file info;
    let m = info.Module_js._module in
    let f = get_file m in
    Hashtbl.add nh m f;
  );
  nh |> Hashtbl.iter (fun m f ->
    assert (get_module_name f = m);
  );
  ih |> Hashtbl.iter (fun _ info ->
    let parsed = info.Module_js.parsed in
    let checked = info.Module_js.checked in
    let required = info.Module_js.required in
    assert (parsed);
    assert (checked || (NameSet.is_empty required));
  );
)

(* helper *)
let typecheck workers files removed unparsed opts timing make_merge_input =
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();
  (* local inference populates context heap, module info heap *)
  Flow_logger.log "Running local inference";
  let timing, inferred =
    with_timer ~opts "Infer" timing (fun () -> infer workers files opts) in

  (* add tracking modules for unparsed files *)
  List.iter (fun (filename, docblock) ->
    Module_js.add_unparsed_info ~options:opts filename docblock
  ) unparsed;

  (* create module dependency graph, warn on dupes etc. *)
  let timing, () = with_timer ~opts "CommitModules" timing (fun () ->
    let filenames = List.fold_left (fun acc (filename, _) ->
      filename::acc
    ) inferred unparsed in
    commit_modules workers ~options:opts filenames removed
  ) in

  (* call supplied function to calculate closure of modules to merge *)
  let timing, merge_input = with_timer ~opts "MakeMergeInput" timing (fun () ->
    make_merge_input inferred
  ) in

  match merge_input with
  | true, to_merge, direct_deps ->
    (* to_merge is the union of inferred (newly inferred files) and the
       transitive closure of all dependents. direct_deps is the subset of
       to_merge which depend on inferred files directly, or whose import
       resolution paths overlap with them. imports within these files must
       be re-resolved before merging.
       Notes:
       - since the dependencies created by module import statements are only
       one level deep, only imports in direct deps need to be re-resolved
       before merging. in contrast, dependencies on imported types themselves
       are transitive along chains of imports, hence the need to re-merge the
       transitive closure contained in to_merge.
       - residue of module import resolution is held in Module_js.InfoHeap.
       residue of type merging is held in SigContextHeap.
       - since to_merge is the transitive closure of dependencies of inferred,
       and direct_deps are dependencies of inferred, all dependencies of
       direct_deps are included in to_merge
      *)
    Flow_logger.log "Re-resolving directly dependent files";
    let timing, _ = with_timer ~opts "ResolveDirectDeps" timing (fun () ->
      Module_js.clear_infos direct_deps;
      FilenameSet.iter (fun f ->
        let cx = ContextHeap.find_unsafe f in
        Module_js.add_module_info ~options:opts cx
      ) direct_deps;
      if Options.is_debug_mode opts then heap_check to_merge;
    ) in
    Flow_logger.log "Calculating dependencies";
    let timing, dependency_graph =
      with_timer ~opts "CalcDeps" timing (fun () ->
        calc_dependencies workers to_merge
      ) in
    let partition = Sort_js.topsort dependency_graph in
    if profile_and_not_quiet opts then Sort_js.log partition;
    let timing = try
      Flow_logger.log "Merging";
      let timing, () = with_timer ~opts "Merge" timing (fun () ->
        merge_strict workers dependency_graph partition opts
      ) in
      if profile_and_not_quiet opts then Gc.print_stat stderr;
      Flow_logger.log "Done";
      timing
    with exc ->
      prerr_endline (Printexc.to_string exc);
      timing
    in
    (* collate errors by origin *)
    collate_errors ();
    timing

  | false, _, _ ->
    (* collate errors by origin *)
    collate_errors ();
    timing

(* sketch of baseline incremental alg:

   Note: this is for strict mode only, which is the easy case,
   since it doesn't require a transitive merge of all modules
   connected via nonlocal type flows.

   Total case over set of modules M:
    1. for all m:M, do local inference on m, producing graph m.g
    2. for all m:M, for all r:m.required,
      perform substitution of r.export into m.g, producing m.g'

   Incremental case, given previously established modules m:
    1. Mt = (touched) reparse of all m:M where m.file has been updated
    2. for all m:Mt do local inference on m, producing m.g
    3. Mx = (export) m:Mt where m.export or m.name have changed
    4. Md = m:M where m.file has been deleted
    5. Mr = (recheck) m:Mt | (M where !empty(m.required & (Mx | Md))
      (all touched modules plus all others whose required sets
       include any export changes or deletions)
    5. for all m:Mr, for all r:m.required,
        perform substitution of r.export into m.g, producing m.g'
*)

(* produce a reverse dependency map for the given fileset.
   keys are modules provided by the files in fileset.
   value for each key is the set of files which require that
   module directly.
 *)
let calc_reverse_deps workers fileset = Module_js.(
  (* distribute requires from a list of files into reverse-dep map *)
  let job = List.fold_left (fun rdmap f ->
    let reqs = (get_module_info f).required in
    NameSet.fold (fun r rdmap ->
      NameMap.add r FilenameSet.(
        match NameMap.get r rdmap with
        | None -> singleton f
        | Some files -> add f files
      ) rdmap
    ) reqs rdmap
  ) in
  (* merge two reverse-dependency maps *)
  let merge = NameMap.merge (fun _ x y ->
    match x, y with
    | Some v, None
    | None, Some v -> Some v
    | Some v, Some w -> Some (FilenameSet.union v w)
    | None, None -> None
  ) in
  MultiWorker.call workers ~job ~merge
    ~neutral: Module_js.NameMap.empty
    ~next: (Bucket.make (FilenameSet.elements fileset))
)

(* given a reverse dependency map (from modules to the files which
   require them), generate the closure of the dependencies of a
   given fileset, using get_module_info to map files to modules
 *)
let dep_closure rdmap fileset = FilenameSet.(
  let rec expand rdmap fileset seen =
    fold (fun f acc ->
      if mem f !seen then acc else (
        seen := add f !seen;
        let m = Module_js.((get_module_info f)._module) in
        add f (match Module_js.NameMap.get m rdmap with
          | None -> acc
          | Some deps -> union acc (expand rdmap deps seen)
        )
      )
    ) fileset empty
  in expand rdmap fileset (ref empty)
)

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.

   Identify the direct and transitive dependents of re-inferred files and
   removed modules.

   - unmodified_files is all unmodified files in the current state
   - inferred_files is all files that have just been through local inference
   - touched_modules is all modules whose infos have just been cleared

   Note that while touched_modules and (the modules provided by) inferred_files
   usually overlap, inferred_files will include providers of new modules, and
   touched_modules will include modules provided by deleted files.

   Return the subset of unmodified_files transitively dependent on changes,
   and the subset directly dependent on them.
*)
let dependent_files workers unmodified_files inferred_files touched_modules =

  (* get reverse dependency map for unmodified files.
     TODO should generate this once on startup, keep required_by
     in module infos and update incrementally on recheck *)
  let reverse_deps = calc_reverse_deps workers unmodified_files in

  (* expand touched_modules to include those provided by new files *)
  let touched_modules = FilenameSet.fold Module_js.(fun file mods ->
    NameSet.add (get_module_name file) mods
  ) inferred_files touched_modules in

  (* files whose resolution paths may encounter newly inferred modules *)
  let resolution_path_files = MultiWorker.call workers
    ~job: (List.fold_left
      (Module_js.resolution_path_dependency inferred_files))
    ~neutral: FilenameSet.empty
    ~merge: FilenameSet.union
    ~next: (Bucket.make (FilenameSet.elements unmodified_files)) in

  (* files that require touched modules directly, or may resolve to
     modules provided by newly inferred files *)
  let direct_deps = Module_js.(NameSet.fold (fun m s ->
    match NameMap.get m reverse_deps with
    | Some files -> FilenameSet.union s files
    | None -> s
    ) touched_modules FilenameSet.empty
  ) |> FilenameSet.union resolution_path_files in

  (* (transitive dependents are re-merged, directs are also re-resolved) *)
  dep_closure reverse_deps direct_deps,
  direct_deps

(* We maintain the following invariant across rechecks: The set of
   `files` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env modified =
  let options = genv.ServerEnv.options in
  let debug = Options.is_debug_mode options in

  (* If foo.js is modified and foo.js.flow exists, then mark foo.js.flow as
   * modified too. This is because sometimes we decide what foo.js.flow
   * provides based on the existence of foo.js *)
  let modified = FilenameSet.fold (fun file modified ->
    if not (Loc.check_suffix file Files_js.flow_ext) &&
      Parsing_service_js.has_ast (Loc.with_suffix file Files_js.flow_ext)
    then FilenameSet.add (Loc.with_suffix file Files_js.flow_ext) modified
    else modified
  ) modified modified in

  let timing = FlowEventLogger.Timing.create () in

  (* track deleted files, remove from modified set *)
  let deleted = FilenameSet.filter (fun f ->
    not (Sys.file_exists (string_of_filename f))
  ) modified in
  let deleted_count = FilenameSet.cardinal deleted in
  let modified = FilenameSet.diff modified deleted in
  let modified_count = FilenameSet.cardinal modified in

  (* log modified and deleted files *)
  if deleted_count + modified_count > 0 then (
    prerr_endlinef "recheck %d modified, %d deleted files"
      modified_count deleted_count;
    let log_files files msg n =
      prerr_endlinef "%s files:" msg;
      let _ = FilenameSet.fold (fun f i ->
        prerr_endlinef "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) files 1
      in ()
    in
    if modified_count > 0 then log_files modified "modified" modified_count;
    if deleted_count > 0 then log_files deleted "deleted" deleted_count
  );

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_asts deleted;

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all options then TypesAllowed else TypesForbiddenByDefault
  ) in

  let use_strict = Options.modules_are_use_strict options in

  Flow_logger.log "Parsing";
  (* reparse modified and added files, updating modified to reflect removal of
     unchanged files *)
  let timing, (modified, (freshparsed, freshparse_skips, freshparse_fail, freshparse_errors)) =
    with_timer ~opts:options "Parsing" timing (fun () ->
      let profile = profile_and_not_quiet options in
      Parsing_service_js.reparse
        ~types_mode ~use_strict ~profile
        genv.ServerEnv.workers modified
    ) in
  let modified_count = FilenameSet.cardinal modified in

  (* clear errors for modified files, deleted files and master *)
  let master_cx = Init_js.get_master_cx options in
  clear_errors ~debug (Context.file master_cx :: FilenameSet.elements modified);
  clear_errors ~debug (FilenameSet.elements deleted);

  (* record reparse errors *)
  let failed_filenames = List.map (fun (file, _) -> file) freshparse_fail in
  save_errors parse_errors failed_filenames freshparse_errors;

  (* get old (unmodified, undeleted) files that were parsed successfully *)
  let old_parsed = env.ServerEnv.files in
  let undeleted_parsed = FilenameSet.diff old_parsed deleted in
  let unmodified_parsed = FilenameSet.diff undeleted_parsed modified in

  if debug then prerr_endlinef
    "recheck: old = %d, del = %d, undel = %d, fresh = %d, unmod = %d"
    (FilenameSet.cardinal old_parsed)
    (FilenameSet.cardinal deleted)
    (FilenameSet.cardinal undeleted_parsed)
    (FilenameSet.cardinal freshparsed)
    (FilenameSet.cardinal unmodified_parsed);

  (* clear contexts and module registrations for modified and deleted files *)
  (* remember deleted modules *)
  let to_clear = FilenameSet.union modified deleted in
  ContextHeap.remove_batch to_clear;
  let removed_modules = Module_js.remove_files to_clear in

  (* TODO elsewhere or delete *)
  Context.remove_all_errors master_cx;

  let dependent_file_count = ref 0 in

  (* recheck *)
  let timing = typecheck
    genv.ServerEnv.workers
    freshparsed
    removed_modules
    (List.rev_append freshparse_fail freshparse_skips)
    options
    timing
    (fun inferred ->
      (* need to merge the closure of inferred files and their deps *)
      let inferred_set = FilenameSet.of_list inferred in

      (* direct_deps are unmodified files which directly depend on
         inferred files or removed modules. all_deps are direct_deps
         plus their dependents (transitive closure) *)
      let all_deps, direct_deps = dependent_files
        genv.ServerEnv.workers
        unmodified_parsed
        inferred_set
        removed_modules
      in

      let n = FilenameSet.cardinal all_deps in
      if n > 0
      then Flow_logger.log "remerge %d dependent files:" n;
      dependent_file_count := n;

      let _ = FilenameSet.fold (fun f i ->
        Flow_logger.log "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) all_deps 1 in
      Flow_logger.log "Merge prep";

      (* clear merge errors for unmodified dependents *)
      FilenameSet.iter (fun file ->
        merge_errors := FilenameMap.remove file !merge_errors;
      ) all_deps;

      (* to_merge is inferred files plus all dependents. prep for re-merge *)
      let to_merge = FilenameSet.union all_deps inferred_set in
      LeaderHeap.remove_batch to_merge;
      SigContextHeap.remove_batch to_merge;
      SharedMem.collect `gentle;

      true,
      FilenameSet.elements to_merge,
      direct_deps
    ) in

  FlowEventLogger.recheck
    ~modified_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~timing;

  let parsed = FilenameSet.union freshparsed unmodified_parsed in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.
    files = parsed;
    errorl = get_errors ();
  }

(* full typecheck *)
let full_check workers ~ordered_libs parse_next opts =
  let timing = FlowEventLogger.Timing.create () in

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all opts then TypesAllowed else TypesForbiddenByDefault
  ) in

  let use_strict = Options.modules_are_use_strict opts in

  let profile = profile_and_not_quiet opts in

  Flow_logger.log "Parsing";
  let timing, (parsed, skipped_files, error_files, errors) =
    with_timer ~opts "Parsing" timing (fun () ->
      Parsing_service_js.parse
        ~types_mode ~use_strict ~profile
        workers parse_next
    ) in
  let error_filenames = List.map (fun (file, _) -> file) error_files in
  save_errors parse_errors error_filenames errors;

  Flow_logger.log "Building package heap";
  let timing, () = with_timer ~opts "PackageHeap" timing (fun () ->
    FilenameSet.iter (fun filename ->
      match filename with
      | Loc.JsonFile str when Filename.basename str = "package.json" ->
        let ast = Parsing_service_js.get_ast_unsafe filename in
        Module_js.add_package str ast
      | _ -> ()
    ) parsed;
  ) in

  let timing = typecheck
    workers
    parsed
    Module_js.NameSet.empty
    (List.rev_append error_files skipped_files)
    opts
    timing
    (fun inferred ->
      (* after local inference and before merge, bring in libraries *)
      (* if any fail to parse, our return value will suppress merge *)
      let lib_files = Init_js.init
        ~options:opts
        ordered_libs
        (fun file errs -> save_errors parse_errors [file] [errs])
        (fun file errs -> save_errors infer_errors [file] [errs])
        (fun file sups ->
          save_suppressions error_suppressions [file] [sups])
      in
      (* Note: if any libs failed, return false and files to report errors for.
         (other errors will be suppressed.)
         otherwise, return true and files to merge *)
      let err_libs = List.fold_left (
        fun acc (file, ok) -> if ok then acc else file :: acc
      ) [] lib_files in
      if err_libs != []
      then false, err_libs, FilenameSet.empty
      else true, inferred, FilenameSet.empty
  ) in

  (timing, parsed)

(* helper - print errors. used in check-and-die runs *)
let print_errors options errors =
  let strip_root = Options.should_strip_root options in
  let root = Options.root options in

  let errors =
    if strip_root then Errors.strip_root_from_errors root errors
    else errors
  in

  if options.Options.opt_json
  then Errors.print_error_json stdout errors
  else
    Errors.print_error_summary
      ~flags:(Options.error_flags options)
      ~strip_root
      ~root
      errors

(* initialize flow server state, including full check *)
let server_init genv =
  let options = genv.ServerEnv.options in

  let ordered_libs, libs = Files_js.init options in

  let get_next_raw = Files_js.make_next_files ~options ~libs in
  let get_next = fun () ->
    get_next_raw () |> List.map (fun file ->
      if Files_js.is_json_file file
      then Loc.JsonFile file
      else Loc.SourceFile file
    )
  in
  let (timing, parsed) =
    full_check genv.ServerEnv.workers ~ordered_libs get_next options in

  SharedMem.init_done();

  let errors = get_errors () in
  if Options.is_check_mode options
  then print_errors options errors;

  (* Return an env that initializes invariants required and maintained by
     recheck, namely that `files` contains files that parsed successfully, and
     `errorl` contains the current set of errors. *)
  timing, { ServerEnv.
    files = parsed;
    libs;
    errorl = errors;
  }
