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
open Constraint_js
open Modes_js

module TI = Type_inference_js
module Module = Module_js

(* per-file error info is a filename and an error list *)
type file_errors = string * Errors_js.error list

(* set of successful files, list of failed files,
 * and parallel list of error lists. *)
type results = SSet.t * string list * Errors_js.error list list

let init_modes opts = Options.(
  modes.debug <- opts.opt_debug;
  modes.verbose <- opts.opt_verbose;
  modes.verbose_indent <- should_indent_verbose opts;
  modes.all <- opts.opt_all;
  modes.weak_by_default <- opts.opt_weak;
  modes.traces <- opts.opt_traces;
  modes.strict <- opts.opt_strict;
  modes.json <- opts.opt_json;
  modes.strip_root <- opts.opt_strip_root;
  modes.quiet <- opts.opt_quiet;
  modes.profile <- opts.opt_profile;
  modes.no_flowlib <- opts.opt_no_flowlib;
  modes.munge_underscores <- opts.opt_munge_underscores;
  (* TODO: confirm that only master uses strip_root, otherwise set it! *)
  Module_js.init opts;
  Files_js.init ~tmp_dir:(Options.temp_dir opts) opts.opt_libs
)

(****************** shared context heap *********************)

(* map from file names to contexts *)
(* NOTE: Entries are cached for performance, since contexts are read a lot more
   than they are written. But this means that proper care must be taken when
   reading contexts: in particular, context graphs have mutable bounds, so they
   must be copied, otherwise bad things will happen. (The cost of copying
   context graphs is presumably a lot less than deserializing contexts, so the
   optimization makes sense. *)
module ContextHeap = SharedMem.WithCache (String) (struct
  type t = context
  let prefix = Prefix.make()
end)

module SigContextHeap = SharedMem.WithCache (String) (struct
  type t = context
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
let parse_errors = ref SMap.empty
(* errors encountered during local inference *)
let infer_errors = ref SMap.empty
(* errors encountered during module commit *)
let module_errors = ref SMap.empty
(* errors encountered during merge *)
let merge_errors = ref SMap.empty
(* error suppressions in the code *)
let error_suppressions = ref SMap.empty
(* aggregate error map, built after check or recheck
   by collate_errors
 *)
let all_errors = ref SMap.empty

(****************** typecheck job helpers *********************)

(* error state handling.
   note: once weve decoupled from hack binary, these will be stored
   in the recurrent env struct, not local state *)
let clear_errors files =
  List.iter (fun file ->
    debug_string (fun () -> spf "clear errors %s" file);
    parse_errors := SMap.remove file !parse_errors;
    infer_errors := SMap.remove file !infer_errors;
    module_errors := SMap.remove file !module_errors;
    merge_errors := SMap.remove file !merge_errors;
    error_suppressions := SMap.remove file !error_suppressions;
  ) files;
  all_errors := SMap.empty

(* helper - save an error set into a global error map.
   clear mapping if errorset is empty *)
let save_errset mapref file errset =
  mapref := if Errors_js.ErrorSet.cardinal errset = 0
    then SMap.remove file !mapref
    else SMap.add file errset !mapref

(* given a reference to a error map (files to errorsets), and
   two parallel lists of such, save the latter into the former. *)
let save_errors mapref files errsets =
  List.iter2 (save_errset mapref) files errsets

let save_errormap mapref errmap =
  SMap.iter (save_errset mapref) errmap

(* given a reference to a error suppression map (files to
   error suppressions), and two parallel lists of such,
   save the latter into the former. *)
let save_suppressions mapref files errsups = Errors_js.(
  List.iter2 (fun file errsup ->
    mapref := if ErrorSuppressions.cardinal errsup = 0
      then SMap.remove file !mapref
      else SMap.add file errsup !mapref
  ) files errsups
)

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs = Errors_js.(

  let distrib_error orig_file error error_map =
    let file = file_of_error error in
    match SMap.get file error_map with
    | None ->
      SMap.add file (ErrorSet.singleton error) error_map
    | Some error_set ->
      if ErrorSet.mem error error_set
      then error_map
      else SMap.add file (ErrorSet.add error error_set) error_map
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

  suppressions := SMap.fold
    (fun _key -> ErrorSuppressions.union)
    !error_suppressions
    ErrorSuppressions.empty;

  let emap = emap
    |> SMap.map (ErrorSet.filter filter_suppressed_error)
    |> SMap.filter (fun k v -> not (ErrorSet.is_empty v)) in

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

let strip_root_from_reason_list root list = Errors_js.(
  List.map (function
    | BlameM (loc, s) -> BlameM (Reason_js.strip_root_from_loc root loc, s)
    | CommentM s -> CommentM s
  ) list
)

let strip_root_from_error root error =
  let level, list, trace_reasons = error in
  let list = strip_root_from_reason_list root list in
  let trace_reasons = strip_root_from_reason_list root trace_reasons in
  level, list, trace_reasons

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
  let lib_files = Files_js.get_lib_files () in
  let append_errset errset errlist =
    List.rev_append (Errors_js.ErrorSet.elements errset) errlist
  in
  let revlist_libs, revlist_others = SMap.fold (
    fun file errset (libs, others) ->
      if SSet.mem file lib_files
      then append_errset errset libs, others
      else libs, append_errset errset others
    ) all ([], [])
  in
  let list = List.rev_append revlist_libs (List.rev revlist_others) in

  (* strip root if specified *)
  if modes.strip_root then (
    let path = FlowConfig.((get_unsafe ()).root) in
    (* TODO verify this is still worth doing, otherwise just List.map it *)
    let ae = Array.of_list list in
    Array.iteri (fun i error ->
      ae.(i) <- strip_root_from_error path error
    ) ae;
    Array.to_list ae
  )
  else list

(* we report parse errors if a file is either a lib file,
   a checked module, or an unchecked file that's used as a
   module by a checked file
 *)
let filter_unchecked_unused errmap =
  let is_imported m = match Module.get_reverse_imports m with
  | Some set -> SSet.cardinal set > 0
  | None -> false
  in
  let lib_files = Files_js.get_lib_files () in
  SMap.filter Module.(fun file _ ->
    SSet.mem file lib_files || (
      let info = get_module_info file in
      info.checked || is_imported info._module
    )
  ) errmap

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors files =
  let all = filter_unchecked_unused !parse_errors in
  let all = SMap.fold distrib_errs !infer_errors all in
  let all = SMap.fold distrib_errs !merge_errors all in
  let all = SMap.fold distrib_errs !module_errors all in
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
  List.fold_left (fun (inferred, errsets, errsuppressions) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: inferred %s in %f" file t)
      (fun () ->
        (*prerr_endlinef "[%d] INFER: %s" (Unix.getpid()) file;*)

        (* infer produces a context for this module *)
        let cx = TI.infer_module file in
        (* register module info *)
        Module.add_module_info cx;
        (* note: save and clear errors and error suppressions before storing
         * cx to shared heap *)
        let errs = cx.errors in
        let suppressions = cx.error_suppressions in
        cx.errors <- Errors_js.ErrorSet.empty;
        cx.error_suppressions <- Errors_js.ErrorSuppressions.empty;
        ContextHeap.add cx.file cx;
        (* add filename, errorset, suppressions *)
        cx.file :: inferred, errs :: errsets, suppressions :: errsuppressions
      )
    with exc ->
      prerr_endlinef "(%d) infer_job THROWS: %s"
        (Unix.getpid()) (fmt_file_exc file exc);
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

let checked m = Module.(
  let info = m |> get_module_file |> unsafe_opt |> get_module_info in
  info.checked
)

(* A file is considered to implement a required module r only if the file is
   registered to provide r and the file is checked. Such a file must be merged
   before any file that requires module r, so this notion naturally gives rise
   to a dependency ordering among files for merging. *)
let implementation_file r =
  if Module.module_exists r && checked r
  then Some (Module.get_file r)
  else None

(* warns on missing required modules
   TODO maybe make this suppressable
 *)
let check_requires cx =
  SSet.iter (fun req ->
    if not (Module.module_exists req)
    then
      let loc = SMap.find_unsafe req cx.require_loc in
      let req =
        if Filename.is_relative req
        then req
        else Filename.basename req
      in
      let m_name = req in
      let reason = Reason_js.mk_reason m_name loc in
      let tvar = Flow_js.mk_tvar cx reason in
      Flow_js.lookup_builtin cx (Reason_js.internal_module_name m_name)
        reason (Some (Reason_js.builtin_reason m_name)) tvar;
  ) cx.required

let mk_copy_of_context cx = { cx with
  graph = IMap.map copy_node cx.graph;
  property_maps = cx.property_maps
}

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
    let cx = mk_copy_of_context (ContextHeap.find_unsafe file) in
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
    let cx = mk_copy_of_context orig_cx in
    Hashtbl.add cached_merge_contexts file cx;
    orig_cx, cx
end

let add_decl (r, cx) declarations =
  match SMap.get r declarations with
  | None -> SMap.add r [cx] declarations
  | Some cxs -> SMap.add r (cx::cxs) declarations

(**********************************)
(* entry point for merging a file *)
(**********************************)
module LeaderHeap = SharedMem.WithCache (String) (struct
  type t = string
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
  List.iter check_requires component_cxs;
  let required = List.fold_left (fun required cx ->
    SSet.fold (fun r -> add_decl (r, cx)) cx.required required
  ) SMap.empty component_cxs in
  let cx = List.hd component_cxs in

  let sig_cache = new sig_context_cache in

  let orig_sig_cxs, sig_cxs, impls, decls =
    SMap.fold (fun r cxs_to
      (orig_sig_cxs, sig_cxs, impls, decls) ->
        match implementation_file r with
        | Some file ->
            let impl sig_cx = List.map (fun cx_to -> sig_cx, r, cx_to) cxs_to in
            begin match cache#find file with
            | Some sig_cx ->
                orig_sig_cxs, sig_cxs,
                List.rev_append (impl sig_cx) impls, decls
            | None ->
                let file = LeaderHeap.find_unsafe file in
                begin match sig_cache#find file with
                | Some sig_cx ->
                    orig_sig_cxs, sig_cxs,
                    List.rev_append (impl sig_cx) impls, decls
                | None ->
                    let orig_sig_cx, sig_cx = sig_cache#read file in
                    orig_sig_cx::orig_sig_cxs, sig_cx::sig_cxs,
                    List.rev_append (impl sig_cx) impls,
                    decls
                end
            end
        | None ->
            (* NOTE: currently we typecheck against libraries only when
               corresponding implementations are not found or not checked. This
               will change in the future. *)
            orig_sig_cxs, sig_cxs, impls, SMap.add r cxs_to decls
    ) required ([], [], [], SMap.empty)
  in

  let orig_master_cx, master_cx = sig_cache#read Files_js.global_file_name in

  TI.merge_component_strict component_cxs sig_cxs impls decls master_cx;
  TI.restore cx orig_sig_cxs orig_master_cx;

  ()

(* Entry point for merging a component *)
let merge_strict_component component =
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
    let errors = cx.errors in
    cx.errors <- Errors_js.ErrorSet.empty;
    SigContextHeap.add file cx;
    file, errors
  )
  else file, Errors_js.ErrorSet.empty

(* Special case of merging a single file. We assume that the file system is in a
   stable state when this function is called, so that we don't need to worry
   about cycles, and can simply read the signatures of dependencies. *)
let merge_strict_file file =
  let cache = new context_cache in
  let cx = cache#read file in
  merge_strict_context cache [cx];
  cx

(* Another special case, similar assumptions as above. *)
(** TODO: handle case when file+contents don't agree with file system state **)
let typecheck_contents contents filename =
  Parsing_service_js.(match do_parse contents filename with
  | OK ast ->
      let cx = TI.infer_ast ast filename true in
      let cache = new context_cache in
      merge_strict_context cache [cx];
      Some cx, cx.errors

  | Err errors ->
      None, errors
  )

let merge_strict_job opts (merged, errsets) components =
  init_modes opts;
  List.fold_left (fun (merged, errsets) component ->
    let file = String.concat "\n\t" component in
    try checktime opts 1.0
      (fun t -> spf "[%d] perf: merged %s in %f" (Unix.getpid()) file t)
      (fun () ->
        (*prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) file;*)
        let file, errors = merge_strict_component component in
        file :: merged, errors :: errsets
      )
    with exc ->
      prerr_endlinef "(%d) merge_strict_job THROWS: [%d] %s\n"
        (Unix.getpid()) (List.length component) (fmt_file_exc file exc);
      (merged, errsets)
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
  let dependents = ref SMap.empty

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
    let leader f = SMap.find_unsafe f leader_map in
    let component f = SMap.find_unsafe f component_map in

    let dependency_dag = SMap.fold (fun f fs dependency_dag ->
      let leader_f = leader f in
      let dep_leader_fs = match SMap.get leader_f dependency_dag with
        | Some dep_leader_fs -> dep_leader_fs
        | _ -> SSet.empty
      in
      let dep_leader_fs = SSet.fold (fun f dep_leader_fs ->
        let f = leader f in
        if f = leader_f then dep_leader_fs else SSet.add f dep_leader_fs
      ) fs dep_leader_fs in
      SMap.add leader_f dep_leader_fs dependency_dag
    ) dependency_graph SMap.empty in

    SMap.iter (fun leader_f dep_leader_fs ->
      let n = SSet.cardinal dep_leader_fs in
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
      if jobs = 0 && !blocked <> 0 then None
      else
        let bucket_size =
          if jobs < procs * max_bucket_size
          then 1 + (jobs / procs)
          else max_bucket_size
        in
        let n = min bucket_size jobs in
        let result = take n |> List.map component in
        Some result

  (* We know when files are done by having jobs return the files they processed,
     and trapping the function that joins results. ;), yeah. *)
  let join =
    let push leader_fs =
      List.iter (fun leader_f ->
        SSet.iter (fun dep_leader_f ->
          let n = (Hashtbl.find blocking dep_leader_f) - 1 in
          (* dep_leader blocked on one less *)
          Hashtbl.replace blocking dep_leader_f n;
          if n = 0 then
            (* one less blocked; add dep_leader_f to stream *)
            (decr blocked; stream := dep_leader_f::!stream)
        ) (SMap.find_unsafe leader_f !dependents)
      ) leader_fs
    in
    fun res acc ->
      let leader_fs, _ = res in
      push leader_fs;
      rev_append_pair res acc

end

let merge_strict workers dependency_graph partition opts =
  (* NOTE: master_cx will only be saved once per server lifetime *)
  let master_cx = Flow_js.master_cx () in
  (* TODO: we probably don't need to save master_cx in ContextHeap *)
  ContextHeap.add Files_js.global_file_name master_cx;
  (* store master signature context to heap *)
  SigContextHeap.add Files_js.global_file_name master_cx;
  (* make a map from component leaders to components *)
  let component_map =
    let result = ref SMap.empty in
    IMap.iter (fun _ components ->
      List.iter (fun component ->
        result := SMap.add (List.hd component) component !result;
      ) components;
    ) partition;
    !result
  in
  (* make a map from files to their component leaders *)
  let leader_map =
    let result = ref SMap.empty in
    component_map |> SMap.iter (fun file component ->
      component |> List.iter (fun file_ ->
        result := SMap.add file_ file !result
      );
    );
    !result
  in
  (* store leaders to a heap; used when rechecking *)
  leader_map |> SMap.iter LeaderHeap.add;
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
        master_cx.file :: files,
        master_cx.errors :: errsets
      ) in
      (* save *)
      save_errors merge_errors files errsets;
    )

(* Calculate module dependencies. Since this involves a lot of reading from
   shared memory, it is useful to parallelize this process (leading to big
   savings in init and recheck times). *)

let calc_dependencies_job =
  List.fold_left (fun deps file ->
    let { Module.required; _ } = Module.get_module_info file in
    let files = SSet.fold (fun r files ->
      match implementation_file r with
      | Some f -> SSet.add f files
      | None -> files
    ) required SSet.empty in
    SMap.add file files deps
  )

let calc_dependencies workers files =
  let deps = MultiWorker.call
    workers
    ~job: calc_dependencies_job
    ~neutral: SMap.empty
    ~merge: SMap.union
    ~next: (Bucket.make files) in
  SMap.map (SSet.filter (fun f -> SMap.mem f deps)) deps

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules inferred removed =
  let errmap = Module.commit_modules inferred removed in
  save_errormap module_errors errmap

(* Sanity checks on InfoHeap and NameHeap. Since this is performance-intensive
   (although it probably doesn't need to be), it is only done under --debug. *)
let heap_check files = Module.(
  let ih = Hashtbl.create 0 in
  let nh = Hashtbl.create 0 in
  files |> List.iter (fun file ->
    assert (get_file file = file);
    let info = get_module_info file in
    Hashtbl.add ih file info;
    let m = info.Module._module in
    let f = get_file m in
    Hashtbl.add nh m f;
  );
  nh |> Hashtbl.iter (fun m f ->
    assert (get_module_name f = m);
  );
  ih |> Hashtbl.iter (fun file info ->
    let parsed = info.Module.parsed in
    let checked = info.Module.checked in
    let required = info.Module.required in
    assert (parsed);
    assert (checked || (SSet.is_empty required));
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
let typecheck workers files removed unparsed opts make_merge_input =
  (* TODO remove after lookup overhaul *)
  Module.clear_filename_cache ();
  (* local inference populates context heap, module info heap *)
  let inferred = infer workers files opts in
  (* add tracking modules for unparsed files *)
  List.iter Module.add_unparsed_info unparsed;
  (* create module dependency graph, warn on dupes etc. *)
  commit_modules inferred removed;
  (* SHUTTLE *)
  (* call supplied function to calculate closure of modules to merge *)
  match make_merge_input inferred with
  | true, to_merge ->
    if opts.Options.opt_debug then heap_check to_merge;
    let dependency_graph = calc_dependencies workers to_merge in
    let partition = Sort_js.topsort dependency_graph in
    if profile_and_not_quiet opts then Sort_js.log partition;
    (if modes.strict then (
      try
        merge_strict workers dependency_graph partition opts;
        if profile_and_not_quiet opts then Gc.print_stat stderr;
      with exc ->
        prerr_endline (Printexc.to_string exc)
     ) else
        failwith "Did you forget to pass the --strict flag?"
    );
    (* collate errors by origin *)
    collate_errors to_merge;
    to_merge
  | false, to_collate ->
    (* collate errors by origin *)
    collate_errors to_collate;
    []

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

   Since the set of modules S is fixed in this computation, memoizing which
   modules M are already known to depend on S greatly speeds up this computation
   (since many files share the same dependencies). This is particularly
   important since this is a near-constant cost for every recheck. Going
   further, this computation can be readily parallelized, since basically it
   does the same check on a large number of files.
*)
let file_depends_on =
  let rec sig_depends_on seen modules memo m =
    match SMap.get m !memo with
    | Some result -> result
    | None ->
        let result = SSet.mem m modules || (
          Module.module_exists m && (
            let f = Module.get_file m in
            not (SSet.mem f !seen) && (
              seen := SSet.add f !seen;
              let { Module.required; _ } = Module.get_module_info f in
              SSet.exists (sig_depends_on seen modules memo) required
            )
          )
        ) in
        memo := SMap.add m result !memo;
        result
  in

  fun modules memo f ->
    let { Module._module; required; _ } = Module.get_module_info f in
    SSet.mem _module modules || (
      let seen = ref SSet.empty in
      SSet.exists (sig_depends_on seen modules memo) required
    )

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.
*)
let deps_job touched_modules files unmodified =
  let memo = ref SMap.empty in
  List.rev_append files
    (List.filter (file_depends_on touched_modules memo) unmodified)

let deps workers unmodified inferred_files removed_modules =
  (* touched modules are all that were inferred, re-inferred or removed *)
  let touched_modules = SSet.fold (fun file mods ->
    SSet.add (Module.get_module_name file) mods
  ) inferred_files removed_modules in
  (* return untouched files that depend on these *)
  set_of_list (MultiWorker.call
    workers
    ~job: (deps_job touched_modules)
    ~neutral: []
    ~merge: List.rev_append
    ~next: (Bucket.make (SSet.elements unmodified)))

(* We maintain the following invariant across rechecks: The keyset of
   `files_info` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env modified =
  let options = genv.ServerEnv.options in
  if not options.Options.opt_strict
  then failwith "Missing -- strict";

  (* filter modified files *)
  let root = Options.root options in
  let config = FlowConfig.get root in
  let modified = SSet.filter (Files_js.wanted config) modified in

  let n = SSet.cardinal modified in
    if n > 0
    then prerr_endlinef "recheck %d files:" n;

  let _ = SSet.fold (fun f i ->
    if n > 0
    then prerr_endlinef "%d/%d: %s" i n f; i + 1) modified 1 in

  (* clear errors for modified files and master *)
  let master_cx = Flow_js.master_cx () in
  clear_errors (master_cx.file :: SSet.elements modified);

  (* track deleted files, remove from modified set *)
  let deleted = SSet.filter (fun f -> not (Sys.file_exists f)) modified in
  let modified = SSet.diff modified deleted in

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_asts deleted;

  (* reparse modified and added files *)
  let freshparsed, freshparse_fail, freshparse_errors =
    Parsing_service_js.reparse genv.ServerEnv.workers modified
      (fun () -> init_modes options)
  in
  save_errors parse_errors freshparse_fail freshparse_errors;

  (* get old (unmodified, undeleted) files that were parsed successfully *)
  let old_parsed = ServerEnv.PathMap.fold (fun k _ a ->
    SSet.add (Path.to_string k) a)
    env.ServerEnv.files_info SSet.empty in
  let undeleted_parsed = SSet.diff old_parsed deleted in
  let unmodified_parsed = SSet.diff undeleted_parsed modified in

  Modes_js.debug_string (fun () -> spf
    "recheck: old = %d, del = %d, undel = %d, fresh = %d, unmod = %d"
    (SSet.cardinal old_parsed)
    (SSet.cardinal deleted)
    (SSet.cardinal undeleted_parsed)
    (SSet.cardinal freshparsed)
    (SSet.cardinal unmodified_parsed));

  (* clear info for modified and deleted files *)
  (* remember deleted modules *)
  let to_clear = SSet.union modified deleted in
  ContextHeap.remove_batch to_clear;
  let removed_modules = Module.remove_files to_clear in

  (* TODO elsewhere or delete *)
  master_cx.errors <- Errors_js.ErrorSet.empty;

  (* recheck *)
  let freshparsed_list = SSet.elements freshparsed in
  typecheck
    genv.ServerEnv.workers
    freshparsed_list
    removed_modules
    freshparse_fail
    options
    (fun inferred ->
      (* need to merge the closure of inferred files and their deps *)
      let inferred_set = set_of_list inferred in
      let unmod_deps = deps genv.ServerEnv.workers
        unmodified_parsed inferred_set removed_modules in

      let n = SSet.cardinal unmod_deps in
        if n > 0
        then prerr_endlinef "remerge %d dependent files:" n;

      let _ = SSet.fold (fun f i ->
        prerr_endlinef "%d/%d: %s" i n f; i + 1) unmod_deps 1 in

      (* clear merge errors for unmodified dependents *)
      SSet.iter (fun file ->
        merge_errors := SMap.remove file !merge_errors;
      ) unmod_deps;

      let to_merge = SSet.union unmod_deps inferred_set in
      LeaderHeap.remove_batch to_merge;
      SigContextHeap.remove_batch to_merge;
      SharedMem.collect `gentle;

      true, SSet.elements to_merge
    ) |> ignore;

  (* for now we populate file_infos with empty def lists *)
  let parsed = SSet.union freshparsed unmodified_parsed in
  let files_info = SSet.fold (fun file info ->
    let file = Path.make file in
    ServerEnv.PathMap.add file Parsing_service.empty_file_info info
  ) parsed ServerEnv.PathMap.empty in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.files_info = files_info; }

(* full typecheck *)
let full_check workers parse_next opts =
  init_modes opts;

  let parsed, error_files, errors =
    Parsing_service_js.parse workers parse_next
      (fun () -> init_modes opts)
  in
  save_errors parse_errors error_files errors;

  let files = SSet.elements parsed in
  let checked = typecheck workers files SSet.empty error_files opts (
    fun inferred ->
      (* after local inference and before merge, bring in libraries *)
      (* if any fail to parse, our return value will suppress merge *)
      let lib_files = Init_js.init
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
      then false, err_libs
      else true, inferred
  ) in

  (parsed, checked)

(* helper - print errors. used in check-and-die runs *)
let print_errors options errors =
  if options.Options.opt_json
  then Errors_js.print_error_json stdout errors
  else
    Errors_js.print_error_summary ~flags:(Options.error_flags options) errors

(* initialize flow server state, including full check *)
let server_init genv env =
  let options = genv.ServerEnv.options in
  let root = Options.root options in

  Files_js.package_json root |> SSet.iter (fun package ->
    let errors = Module_js.add_package package in
    match errors with
    | None -> ()
    | Some error ->
      save_errors infer_errors [package] [error]
  );

  let get_next = Files_js.make_next_files root in
  let (parsed, checked) =
    full_check genv.ServerEnv.workers get_next options in

  (* for now we populate file_infos with empty def lists *)
  let files_info = SSet.fold (fun file info ->
    let file = Path.make file in
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
    { env with ServerEnv.errorl = errors }
  ) else (
    env
  )

(* single command entry point: takes a list of paths,
 * parses and checks serially, prints errs to stdout.
 *)
let single_main (paths : string list) options =
  let get_next = Files_js.make_next_files (Path.make (List.hd paths)) in
  let _ = full_check None get_next options in
  print_errors options (get_errors ())
