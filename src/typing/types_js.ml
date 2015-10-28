(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module drives the type checker. Currently the typechecker is hooked up
   to Hack's "single_typecheck mode"; integration with "server mode" is in the
   works. What this means in practical terms is that currently Flow does a
   whole-program analysis for a set of files, reports errors,
   and...exits. In the near future, we should be able to do modular
   analysis, remember results, recompute them when necessary, etc.---essentially
   mimic all the sweet ways Hack scales. *)

open Utils
open Utils_js
open Modes_js

module TI = Type_inference_js

(* per-file error info is a filename and an error list *)
type file_errors = string * Errors_js.error list

(* set of successful files, list of failed files,
 * and parallel list of error lists. *)
type results = SSet.t * string list * Errors_js.error list list

let init_modes opts = Options.(
  modes.traces <- opts.opt_traces;
  modes.strip_root <- opts.opt_strip_root;
  modes.quiet <- opts.opt_quiet;
  modes.profile <- opts.opt_profile;
  (* TODO: confirm that only master uses strip_root, otherwise set it! *)
  Module_js.init opts;
  Files_js.init
    ~tmp_dir:(Options.temp_dir opts)
    ~include_default_libs:(not opts.opt_no_flowlib)
    opts.opt_libs
)

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
(* errors encountered during parsing.
   Note: both @flow and non-@flow files are parsed,
   and their errors are retained. However, only
   files referenced as modules from @flow files
   have their parse errors reported. collate_errors
   does the filtering *)
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
  mapref := if Errors_js.ErrorSet.cardinal errset = 0
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
let save_suppressions mapref files errsups = Errors_js.(
  List.iter2 (fun file errsup ->
    mapref := if ErrorSuppressions.cardinal errsup = 0
      then FilenameMap.remove file !mapref
      else FilenameMap.add file errsup !mapref
  ) files errsups
)

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs = Errors_js.(

  let distrib_error orig_file error error_map =
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
let filter_suppressed_errors emap = Errors_js.(
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
    |> FilenameMap.filter (fun k v -> not (ErrorSet.is_empty v)) in

  (* For each unused suppression, create an error *)
  let unused_suppression_errors = List.fold_left
    (fun errset loc ->
      let reason = Reason_js.mk_reason "Error suppressing comment" loc in
      let err = Flow_js.new_error [(reason, "Unused suppression")] in
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
    List.rev_append (Errors_js.ErrorSet.elements errset) errlist
  in
  let revlist_libs, revlist_others = FilenameMap.fold (
    fun file errset (libs, others) ->
      if is_lib_file file
      then append_errset errset libs, others
      else libs, append_errset errset others
    ) all ([], [])
  in
  List.rev_append revlist_libs (List.rev revlist_others)

(* we report parse errors if a file is either a lib file,
   a checked module, or an unchecked file that's used as a
   module by a checked file
 *)
let filter_unchecked_unused errmap = Module_js.(
  let is_imported m = match get_reverse_imports m with
  | Some set -> NameSet.cardinal set > 0
  | None -> false
  in
  FilenameMap.filter (fun file _ ->
    is_lib_file file || (
      let info = get_module_info file in
      info.checked || is_imported info._module
    )
  ) errmap
)

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors files =
  let all = filter_unchecked_unused !parse_errors in
  let all = FilenameMap.fold distrib_errs !infer_errors all in
  let all = FilenameMap.fold distrib_errs !merge_errors all in
  let all = FilenameMap.fold distrib_errs !module_errors all in
  all_errors := all

let profile_and_not_quiet opts =
  Options.(opts.opt_profile && not opts.opt_quiet)

let wraptime opts pred msg f =
  if profile_and_not_quiet opts then time pred msg f
  else f()

let checktime opts limit msg f =
  wraptime opts (fun t -> t > limit) msg f

let logtime opts msg f =
  wraptime opts (fun _ -> true) msg f

(* local inference job:
   takes list of filenames, accumulates into parallel lists of
   filenames, error sets *)
let infer_job opts (inferred, errsets, errsuppressions) files =
  init_modes opts;
  let metadata = { Context.
    checked = Options.all opts;
    weak = Options.weak_by_default opts;
    munge_underscores = Options.should_munge_underscores opts;
    verbose = Options.verbose opts;
    is_declaration_file = false;
  } in
  List.fold_left (fun (inferred, errsets, errsuppressions) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: inferred %s in %f" (string_of_filename file) t)
      (fun () ->
        (*prerr_endlinef "[%d] INFER: %s" (Unix.getpid()) file;*)

        (* infer produces a context for this module *)
        let cx = TI.infer_module ~metadata file in
        (* register module info *)
        Module_js.add_module_info cx;
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
      prerr_endlinef "(%d) infer_job THROWS: %s"
        (Unix.getpid()) (fmt_file_exc (string_of_filename file) exc);
      inferred, errsets, errsuppressions
  ) (inferred, errsets, errsuppressions) files

let rev_append_pair (x1, y1) (x2, y2) =
  (List.rev_append x1 x2, List.rev_append y1 y2)

let rev_append_triple (x1, y1, z1) (x2, y2, z2) =
  (List.rev_append x1 x2, List.rev_append y1 y2, List.rev_append z1 z2)

(* local type inference pass.
   Returns a set of sucessfully inferred files.
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
    let loc = SMap.find_unsafe r (Context.require_loc cx) in
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
    let cx = Context.copy_of_context (ContextHeap.find_unsafe file) in
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
    let orig_cx = SigContextHeap.find_unsafe file in
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
let merge_strict_context cache component_cxs =
  let required = List.fold_left (fun required cx ->
    SSet.fold (fun r ->
      let resolved_r = Module_js.find_resolved_module (Context.file cx) r in
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
          if info.checked then
            (* checked implementation exists *)
            let impl sig_cx = sig_cx, r, info._module, cx_to in
            begin match cache#find file with
            | Some sig_cx ->
                orig_sig_cxs, sig_cxs,
                (impl sig_cx) :: impls, decls
            | None ->
                let file = LeaderHeap.find_unsafe file in
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
            orig_sig_cxs, sig_cxs, impls, (r, info._module, cx_to) :: decls
      | None ->
          (* implementation doesn't exist *)
          orig_sig_cxs, sig_cxs, impls, (r, resolved_r, cx_to) :: decls
      )
    ) ([], [], [], []) required
  in

  let orig_master_cx, master_cx = sig_cache#read Loc.Builtins in

  TI.merge_component_strict component_cxs sig_cxs impls decls master_cx;
  TI.restore cx orig_sig_cxs orig_master_cx;

  ()

(* Entry point for merging a component *)
let merge_strict_component (component: filename list) =
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

    merge_strict_context cache component_cxs;

    Flow_js.ContextOptimizer.sig_context component_cxs;
    let cx = List.hd component_cxs in
    let errors = Context.errors cx in
    Context.remove_all_errors cx;
    SigContextHeap.add file cx;
    file, errors
  )
  else file, Errors_js.ErrorSet.empty

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
let typecheck_contents ?verbose contents filename =
  let timing = FlowEventLogger.Timing.create () in

  (* always enable types when checking an individual file *)
  let types_mode = Parsing_service_js.TypesAllowed in
  let timing, parse_result = with_timer "Parsing" timing (fun () ->
    Parsing_service_js.do_parse
      ~fail:false ~types_mode
      contents filename
  ) in
  match parse_result with
  | OK (ast, info) ->
      (* defaults *)
      let metadata = { Context.
        checked = true;
        weak = false;
        munge_underscores = false; (* TODO: read from .flowconfig? *)
        verbose;
        is_declaration_file = false;
      } in
      (* apply overrides from the docblock *)
      let metadata = TI.apply_docblock_overrides metadata info in

      let timing, cx = with_timer "Infer" timing (fun () ->
        TI.infer_ast
          ~gc:false ~metadata ~filename ~module_name:(Modulename.String "-") ast
      ) in

      let cache = new context_cache in
      let timing, () = with_timer "Merge" timing (fun () ->
        merge_strict_context cache [cx]
      ) in
      timing, Some cx, Context.errors cx, parse_result

  | Err errors ->
      timing, None, errors, parse_result

type merge_job_status =
| MergeSuccess of filename
| MergeFailure of filename

let filename_of_merge_job_status = function
  | MergeSuccess fn -> fn
  | MergeFailure fn -> fn

let add_successful_filename merge_job_status fns = match merge_job_status with
  | MergeSuccess fn -> fn::fns
  | MergeFailure fn -> fns

let merge_strict_job opts (merged, errsets) (components: filename list list) =
  init_modes opts;
  List.fold_left (fun (merged, errsets) (component: filename list) ->
    let files = component
    |> List.map string_of_filename
    |> String.concat "\n\t"
    in
    try checktime opts 1.0
      (fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) files t)
      (fun () ->
        (*prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) file;*)
        let file, errors = merge_strict_component component in
        MergeSuccess(file) :: merged, errors :: errsets
      )
    with exc ->
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc files exc);
      (MergeFailure(List.hd component) :: merged, errsets)
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
      push (List.map filename_of_merge_job_status leader_fs);
      rev_append_pair res acc

end

let merge_strict workers dependency_graph partition opts =
  (* NOTE: master_cx will only be saved once per server lifetime *)
  let master_cx = Flow_js.master_cx () in
  (* TODO: we probably don't need to save master_cx in ContextHeap *)
  ContextHeap.add Loc.Builtins master_cx;
  (* store master signature context to heap *)
  SigContextHeap.add Loc.Builtins master_cx;
  (* make a map from component leaders to components *)
  let component_map =
    let result = ref FilenameMap.empty in
    IMap.iter (fun _ components ->
      List.iter (fun component ->
        result := FilenameMap.add (List.hd component) component !result;
      ) components;
    ) partition;
    !result
  in
  (* make a map from files to their component leaders *)
  let leader_map =
    let result = ref FilenameMap.empty in
    component_map |> FilenameMap.iter (fun file component ->
      component |> List.iter (fun file_ ->
        result := FilenameMap.add file_ file !result
      );
    );
    !result
  in
  (* store leaders to a heap; used when rechecking *)
  leader_map |> FilenameMap.iter LeaderHeap.add;
  logtime opts
    (fun t -> spf "merged (strict) in %f" t)
    (fun () ->
      (* returns parallel lists of filenames and errorsets *)
      let (statuses, errsets) = MultiWorker.call_dynamic
        workers
        ~job: (merge_strict_job opts)
        ~neutral: ([], [])
        ~merge: MergeStream.join
        ~next: (MergeStream.make dependency_graph leader_map component_map) in
      let files = List.fold_right add_successful_filename statuses [] in
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
  FilenameMap.map (FilenameSet.filter (fun f ->
    FilenameMap.mem f deps
  )) deps

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules ?(debug=false) inferred removed =
  let errmap = Module_js.commit_modules ~debug inferred removed in
  save_errormap module_errors errmap

(* Sanity checks on InfoHeap and NameHeap. Since this is performance-intensive
   (although it probably doesn't need to be), it is only done under --debug. *)
let heap_check files = Module_js.(
  let ih = Hashtbl.create 0 in
  let nh = Hashtbl.create 0 in
  files |> List.iter (fun file ->
    let m_file = get_file (Modulename.Filename file) in
    if not (Loc.check_suffix m_file FlowConfig.flow_ext)
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
  ih |> Hashtbl.iter (fun file info ->
    let parsed = info.Module_js.parsed in
    let checked = info.Module_js.checked in
    let required = info.Module_js.required in
    assert (parsed);
    assert (checked || (NameSet.is_empty required));
  );
  (* *)
)

(* helper *)
(* make_merge_input takes list of files produced by infer, and
   returns (true, list of files to merge (typically an expansion)),
   or (false, list of files with errors). If the latter, merge is
   not performed.
   return a list of files that have been checked.
 *)
let typecheck workers files removed unparsed opts timing make_merge_input =
  let debug = Options.is_debug_mode opts in
  (* TODO remove after lookup overhaul *)
  Module_js.clear_filename_cache ();
  (* local inference populates context heap, module info heap *)
  Flow_logger.log "Running local inference";
  let timing, inferred =
    with_timer ~opts "Infer" timing (fun () -> infer workers files opts) in

  (* add tracking modules for unparsed files *)
  let force_check = Options.all opts in
  List.iter (Module_js.add_unparsed_info ~force_check) unparsed;

  (* create module dependency graph, warn on dupes etc. *)
  let (timing, ()) = with_timer ~opts "CommitModules" timing (fun () ->
    commit_modules ~debug inferred removed
  ) in

  (* SHUTTLE *)
  (* call supplied function to calculate closure of modules to merge *)
  let (timing, merge_input) = with_timer ~opts "MakeMergeInput" timing (fun () ->
    make_merge_input inferred
  ) in
  match merge_input with
  | true, to_merge, diff ->
    Module_js.clear_infos diff;
    FilenameSet.iter (fun f ->
      let cx = ContextHeap.find_unsafe f in
      Module_js.add_module_info cx
    ) diff;
    if debug then heap_check to_merge;
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
      timing
    with
    | SharedMem.Out_of_shared_memory as exn -> raise exn
    | exc ->
        prerr_endline (Printexc.to_string exc);
        timing in
    (* collate errors by origin *)
    collate_errors to_merge;
    timing, to_merge
  | false, to_collate, _ ->
    (* collate errors by origin *)
    collate_errors to_collate;
    timing, []

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

(* Given a module M,
   - required(M) is the set of all modules required by M

   Given a module M and a set of modules S,
   - M depends on S if M is in S, or any module in required(M) depends on S.

   This computation can be readily parallelized, since basically it does the
   same check on a large number of files.

   Furthermore, since the set of modules S is fixed in this computation,
   memoizing which modules M are already known to depend on S can speed up this
   computation. But memoization is tricky. As the computation is set up, results
   can go from false to true: we're searching for the existence of paths between
   nodes in a graph via DFS, and at some point a path shows up, even though
   previous recursive calls that explored a part of the graph may not have
   discovered a path. Currently, we choose a simple memoization strategy that
   effectively starts a new DFS per module M that memoizes its result only at
   the end of the DFS. A more aggressive strategy that also works is memoizing
   intermediate results when true (but not false), but we leave that trick for
   later in the interests of simplicity.
*)
let file_depends_on = Module_js.(
  let rec sig_depends_on seen modules memo m =
    NameSet.mem m modules || (
      module_exists m && (
        let f = get_file m in
        match FilenameMap.get f !memo with
        | Some result -> result
        | None ->
          not (FilenameSet.mem f !seen) && (
            seen := FilenameSet.add f !seen;
            let { required; _ } = get_module_info f in
            NameSet.exists (sig_depends_on seen modules memo) required
          )
      )
    )
  in

  fun modules memo f ->
    let { _module; required; _ } = get_module_info f in
    let result = NameSet.mem _module modules || (
      let seen = ref FilenameSet.empty in
      NameSet.exists (sig_depends_on seen modules memo) required
    ) in
    memo := FilenameMap.add f result !memo;
    result
)

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.
*)
let deps_job touched_modules files unmodified =
  let memo = ref FilenameMap.empty in
  List.rev_append files
    (List.filter (file_depends_on touched_modules memo) unmodified)

let deps workers unmodified inferred_files removed_modules =
  let resolution_path_files = MultiWorker.call
    workers
    ~job: (List.fold_left (Module_js.resolution_path_dependency inferred_files))
    ~neutral: FilenameSet.empty
    ~merge: FilenameSet.union
    ~next: (Bucket.make (FilenameSet.elements unmodified)) in

  (* add files with import resolution dependencies to inferred_files *)
  let inferred_files = FilenameSet.union resolution_path_files inferred_files in

  (* touched modules are all that were inferred, re-inferred or removed *)
  let touched_modules = FilenameSet.fold (fun file mods ->
    Module_js.(NameSet.add (get_module_name file) mods)
  ) inferred_files removed_modules in

  (* return untouched files that depend on these *)
  let result_list = MultiWorker.call
    workers
    ~job: (deps_job touched_modules)
    ~neutral: []
    ~merge: List.rev_append
    ~next: (Bucket.make (FilenameSet.elements unmodified)) in

  List.fold_left (fun acc file ->
    FilenameSet.add file acc
  ) FilenameSet.empty result_list


(* We maintain the following invariant across rechecks: The keyset of
   `files_info` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env modified =
  let options = genv.ServerEnv.options in
  let debug = Options.is_debug_mode options in

  let root = Options.root options in
  let config = FlowConfig.get root in

  (* If foo.js is modified and foo.js.flow exists, then mark foo.js.flow as
   * modified too. This is because sometimes we decide what foo.js.flow
   * provides based on the existence of foo.js *)
  let modified = FilenameSet.fold (fun file modified ->
    if not (Loc.check_suffix file FlowConfig.flow_ext) &&
      Parsing_service_js.has_ast (Loc.with_suffix file FlowConfig.flow_ext)
    then FilenameSet.add (Loc.with_suffix file FlowConfig.flow_ext) modified
    else modified
  ) modified modified in

  let timing = FlowEventLogger.Timing.create () in

  (* filter modified files *)
  let modified = FilenameSet.filter (fun file ->
    Files_js.wanted config (string_of_filename file)
  ) modified in

  let n = FilenameSet.cardinal modified in
    if n > 0
    then prerr_endlinef "recheck %d files:" n;

  let _ = FilenameSet.fold (fun f i ->
    if n > 0 then prerr_endlinef "%d/%d: %s" i n (string_of_filename f);
    i + 1
  ) modified 1 in

  (* clear errors for modified files and master *)
  let master_cx = Flow_js.master_cx () in
  clear_errors ~debug (Context.file master_cx :: FilenameSet.elements modified);

  (* track deleted files, remove from modified set *)
  let deleted = FilenameSet.filter (fun f ->
    not (Sys.file_exists (string_of_filename f))
  ) modified in
  let modified = FilenameSet.diff modified deleted in

  let modified_count = FilenameSet.cardinal modified in
  let deleted_count = FilenameSet.cardinal deleted in

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_asts deleted;

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all options then TypesAllowed else TypesForbiddenByDefault
  ) in

  Flow_logger.log "Parsing";
  (* reparse modified and added files *)
  let timing, (freshparsed, freshparse_fail, freshparse_errors) =
    with_timer ~opts:options "Parsing" timing (fun () ->
      Parsing_service_js.reparse ~types_mode genv.ServerEnv.workers modified
        (fun () -> init_modes options)
    ) in
  save_errors parse_errors freshparse_fail freshparse_errors;

  (* get old (unmodified, undeleted) files that were parsed successfully *)
  let old_parsed = ServerEnv.PathMap.fold (fun k _ a ->
    FilenameSet.add (Loc.SourceFile (Path.to_string k)) a
  ) env.ServerEnv.files_info FilenameSet.empty in
  let undeleted_parsed = FilenameSet.diff old_parsed deleted in
  let unmodified_parsed = FilenameSet.diff undeleted_parsed modified in

  if debug then prerr_endlinef
    "recheck: old = %d, del = %d, undel = %d, fresh = %d, unmod = %d"
    (FilenameSet.cardinal old_parsed)
    (FilenameSet.cardinal deleted)
    (FilenameSet.cardinal undeleted_parsed)
    (FilenameSet.cardinal freshparsed)
    (FilenameSet.cardinal unmodified_parsed);

  (* clear info for modified and deleted files *)
  (* remember deleted modules *)
  let to_clear = FilenameSet.union modified deleted in
  ContextHeap.remove_batch to_clear;
  let removed_modules = Module_js.remove_files to_clear in

  (* TODO elsewhere or delete *)
  Context.remove_all_errors master_cx;

  let dependent_file_count = ref 0 in

  (* recheck *)
  let (timing, _) = typecheck
    genv.ServerEnv.workers
    freshparsed
    removed_modules
    freshparse_fail
    options
    timing
    (fun inferred ->
      (* need to merge the closure of inferred files and their deps *)
      let inferred_set = List.fold_left (fun acc file ->
        FilenameSet.add file acc
      ) FilenameSet.empty inferred in
      let unmod_deps = deps genv.ServerEnv.workers
        unmodified_parsed inferred_set removed_modules in

      let n = FilenameSet.cardinal unmod_deps in
      if n > 0
      then Flow_logger.log "remerge %d dependent files:" n;
      dependent_file_count := n;

      let _ = FilenameSet.fold (fun f i ->
        Flow_logger.log "%d/%d: %s" i n (string_of_filename f);
        i + 1
      ) unmod_deps 1 in
      Flow_logger.log "Merging";

      (* clear merge errors for unmodified dependents *)
      FilenameSet.iter (fun file ->
        merge_errors := FilenameMap.remove file !merge_errors;
      ) unmod_deps;

      let to_merge = FilenameSet.union unmod_deps inferred_set in
      LeaderHeap.remove_batch to_merge;
      SigContextHeap.remove_batch to_merge;
      SharedMem.collect `gentle;

      true, FilenameSet.elements to_merge, unmod_deps
    ) in

  FlowEventLogger.recheck
    ~modified_count
    ~deleted_count
    ~dependent_file_count:!dependent_file_count
    ~timing;

  (* for now we populate file_infos with empty def lists *)
  let parsed = FilenameSet.union freshparsed unmodified_parsed in
  let files_info = FilenameSet.fold (fun file info ->
    let file = Path.make (string_of_filename file) in
    ServerEnv.PathMap.add file Parsing_service.empty_file_info info
  ) parsed ServerEnv.PathMap.empty in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.files_info = files_info; }

(* full typecheck *)
let full_check workers parse_next opts =
  init_modes opts;
  let verbose = Options.verbose opts in

  let timing = FlowEventLogger.Timing.create () in

  (* force types when --all is set, but otherwise forbid them unless the file
     has @flow in it. *)
  let types_mode = Parsing_service_js.(
    if Options.all opts then TypesAllowed else TypesForbiddenByDefault
  ) in

  Flow_logger.log "Parsing";
  let timing, (parsed, error_files, errors) =
    with_timer ~opts "Parsing" timing (fun () ->
      Parsing_service_js.parse ~types_mode workers parse_next
        (fun () -> init_modes opts)
    ) in
  save_errors parse_errors error_files errors;

  let timing, checked = typecheck
    workers
    parsed
    Module_js.NameSet.empty
    error_files
    opts
    timing
    (fun inferred ->
      (* after local inference and before merge, bring in libraries *)
      (* if any fail to parse, our return value will suppress merge *)
      let lib_files = Init_js.init
        ~verbose
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

  (timing, parsed, checked)

(* helper - print errors. used in check-and-die runs *)
let print_errors options errors =
  let errors =
    if Options.should_strip_root options then (
      let root = FlowConfig.((get_unsafe ()).root) in
      Errors_js.strip_root_from_errors root errors
    )
    else errors
  in

  if options.Options.opt_json
  then Errors_js.print_error_json stdout errors
  else
    Errors_js.print_error_summary
      ~flags:(Options.error_flags options)
      ~root:(Options.root options)
      errors

(* initialize flow server state, including full check *)
let server_init genv env =
  let options = genv.ServerEnv.options in
  let root = Options.root options in

  Files_js.package_json root |> SSet.iter (fun package ->
    let errors = Module_js.add_package package in
    match errors with
    | None -> ()
    | Some error ->
      (* TODO: add PackageFile type? *)
      save_errors infer_errors [Loc.SourceFile package] [error]
  );

  let get_next_raw = Files_js.make_next_files root in
  let get_next = fun () ->
    get_next_raw () |> List.map (fun file -> Loc.SourceFile file)
  in
  let (timing, parsed, checked) =
    full_check genv.ServerEnv.workers get_next options in

  (* for now we populate file_infos with empty def lists *)
  let files_info = FilenameSet.fold (fun file info ->
    let file = Path.make (string_of_filename file) in
    ServerEnv.PathMap.add file Parsing_service.empty_file_info info
  ) parsed ServerEnv.PathMap.empty in

  (* We ensure an invariant required by recheck, namely that the keyset of
     `files_info` contains files that parsed successfully. *)
  (* NOTE: unused fields are left in their initial empty state *)
  let env = { env with ServerEnv.files_info = files_info; } in

  SharedMem.init_done();

  if Options.is_check_mode options
  then (
    let errors = get_errors () in
    print_errors options errors;
    timing, { env with ServerEnv.errorl = errors }
  ) else (
    timing, env
  )
