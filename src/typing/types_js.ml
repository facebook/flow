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
  (* TODO: confirm that only master uses strip_root, otherwise set it! *)
  Module_js.init opts;
  Files_js.init opts.opt_libs
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
    merge_errors := SMap.remove file !merge_errors;
    module_errors := SMap.remove file !module_errors;
    error_suppressions := SMap.remove file !error_suppressions;
  ) files

(* given a reference to a error map (files to errorsets), and
   two parallel lists of such, save the latter into the former. *)
let save_errors_or_suppressions mapref files errsets =
  List.iter2 (fun file errset ->
    mapref := SMap.add file errset !mapref
  ) files errsets

let save_errormap mapref errmap =
  SMap.iter (fun file errset ->
    mapref := SMap.add file errset !mapref
  ) errmap

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs _ eset emap = Errors_js.(
  ErrorSet.fold (fun e emap ->
    let file = file_of_error e in
    let errs = match SMap.get file emap with
    | Some set -> ErrorSet.add e set
    | None -> ErrorSet.singleton e in
    SMap.add file errs emap
  ) eset emap
)

(* Given all the errors as a map from file => errorset
 * 1) Filter out the suppressed errors from the error sets
 * 2) Remove files with empty errorsets from the map
 * 3) Add errors for unused suppressions
 * 4) Properly distribute the new errors
 *)
let filter_suppressed_errors = Errors_js.(
  let suppressions = ref ErrorSuppressions.empty in

  let filter_suppressed_error error =
    let (suppressed, sups) = ErrorSuppressions.check error !suppressions in
    suppressions := sups;
    not suppressed

  in fun emap ->
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

let strip_root_from_reason_list root list =
  List.map (
    fun (reason, s) -> (Reason_js.strip_root reason root, s)
  ) list

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

let wraptime opts pred msg f =
  if opts.Options.opt_quiet || not opts.Options.opt_profile then f ()
  else time pred msg f

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
      (fun t -> spf "perf: inferred %S in %f" file t)
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
        ~next: (Bucket.make_20 files) in
      save_errors_or_suppressions infer_errors files errors;
      save_errors_or_suppressions error_suppressions files suppressions;

      files
    )

let checked m = Module.(
  let info = m |> get_module_file |> unsafe_opt |> get_module_info in
  info.checked
)

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

(* Merging involves looking up a lot of contexts, and due to the graph-structure
   of dependencies, the same context may be looked up multiple times. We already
   cache contexts in the shared memory for performance, but context graphs need
   to be copied because they have mutable bounds. We maintain an additional
   cache of local copies. Mutating bounds in local copies of context graphs is
   not only OK, but we rely on it during merging, so it is both safe and
   necessary to cache the local copies. As a side effect, this probably helps
   performance too by avoiding redundant copying. *)
class context_cache = object
  val cached_infer_contexts = Hashtbl.create 0

  (* find a context in the cache *)
  method find file =
    try Some (Hashtbl.find cached_infer_contexts file)
    with _ -> None

  (* read a context from shared memory, copy its graph, and cache the context *)
  method read file =
    let cx = ContextHeap.find_unsafe file in
    let cx = { cx with graph = IMap.map copy_node cx.graph } in
    Hashtbl.add cached_infer_contexts file cx;
    cx

end

let add_decl (r, cx) declarations =
  match SMap.get r declarations with
  | None -> SMap.add r [cx] declarations
  | Some cxs -> SMap.add r (cx::cxs) declarations

let merge_decls =
  SMap.merge (fun r cxs1 cxs2 -> match cxs1, cxs2 with
    | None, None -> None
    | Some cxs, None | None, Some cxs -> Some cxs
    | Some cxs1, Some cxs2 -> Some (List.rev_append cxs1 cxs2)
  )

(* Merging requires for a context returns a dependency graph: (a) the set of
   contexts of transitive strict requires for that context, (b) the set of edges
   between these and from these to the context. Contexts are cached. Note also
   that contexts are updated by side effect, so transitive merging is only
   performed when a context is first read and placed in the cache. An
   interesting property of this procedure is that it gracefully handles cycles;
   by delaying the actual substitutions, it can detect cycles via caching. *)
let rec merge_requires cache cx rs =
  SSet.fold (fun r (cxs, implementations, declarations) ->
    if Module.module_exists r && checked r then
      let file = Module.get_file r in
      match cache#find file with
      | Some cx_ ->
          cxs,
          (cx_, cx)::implementations,
          declarations
      | None ->
          let cx_ = cache#read file in
          let (cxs_, implementations_, declarations_) =
            merge_requires cache cx_ cx_.strict_required in
          List.rev_append cxs_ (cx_::cxs),
          List.rev_append implementations_ ((cx_,cx)::implementations),
          merge_decls declarations_ declarations
    else
      cxs,
      implementations,
      add_decl (r, cx) declarations
  ) rs ([],[],SMap.empty)

(* To merge results for a context, check for the existence of its requires,
   compute the dependency graph (via merge_requires), and then compute
   substitutions (via merge_module_strict). A merged context is returned. *)
let merge_strict_context cache cx master_cx =
  if cx.checked then (
    check_requires cx;

    let cxs, impls, decls = merge_requires cache cx cx.required in
    TI.merge_module_strict cx cxs impls decls master_cx
  ) else (
    (* do nothing on unchecked files *)
  );
  cx

(**********************************)
(* entry point for merging a file *)
(**********************************)
let merge_strict_file file =
  let cache = new context_cache in
  (* always use cache to read contexts, instead of directly
     using ContextHeap; otherwise bad things will happen. *)
  let cx = cache#read file in
  let master_cx = cache#read (Files_js.get_flowlib_root ()) in
  merge_strict_context cache cx master_cx

let typecheck_contents contents filename =
  Parsing_service_js.(match do_parse contents filename with
  | OK ast ->
      let cx = TI.infer_ast ast filename true in
      let cache = new context_cache in
      let master_cx = cache#read (Files_js.get_flowlib_root ()) in
      Some (merge_strict_context cache cx master_cx), cx.errors
  | Err errors ->
      None, errors
  )

(* *)
let merge_strict_job opts (merged, errsets) files =
  init_modes opts;
  List.fold_left (fun (merged, errsets) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: merged %S in %f" file t)
      (fun () ->
        (*prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) file;*)
        let cx = merge_strict_file file in
        cx.file :: merged, cx.errors :: errsets
      )
    with exc ->
      prerr_endlinef "(%d) merge_strict_job THROWS: %s\n"
        (Unix.getpid()) (fmt_file_exc file exc);
      (merged, errsets)
  ) (merged, errsets) files

(* *)
let merge_strict workers files opts =
  logtime opts
    (fun t -> spf "merged (strict) %d files in %f" (List.length files) t)
    (fun () ->
      (* NOTE: master_cx will only be saved once per server lifetime *)
      let master_cx = Flow_js.master_cx () in
      ContextHeap.add master_cx.file master_cx;
      (* returns parallel lists of filenames and errorsets *)
      let (files, errsets) = MultiWorker.call
        workers
        ~job: (merge_strict_job opts)
        ~neutral: ([], [])
        ~merge: rev_append_pair
        ~next: (Bucket.make_20 files) in
      (* collect master context errors *)
      let (files, errsets) = (
        master_cx.file :: files,
        master_cx.errors :: errsets
      ) in
      (* save *)
      save_errors_or_suppressions merge_errors files errsets
    )

(* *)
let merge_nonstrict partition opts =
  logtime opts
    (fun t -> spf "merged (nonstrict) %d files in %f"
      (List.length (List.flatten partition)) t)
    (fun () ->
      (* merge all modules by partition *)
      let (files, errsets) = List.fold_left (fun acc file_list ->
        try
          let cx_list = List.map ContextHeap.find_unsafe file_list in
          TI.merge_module_list cx_list;
          List.iter check_requires cx_list;
          List.fold_left (fun (files, errsets) cx ->
            (cx.file :: files, cx.errors :: errsets)
          ) acc cx_list
        with exc ->
          let files = Printf.sprintf "\n%s\n" (String.concat "\n" file_list) in
          prerr_endlinef "(%d) merge_module THROWS: %s\n"
            (Unix.getpid()) (fmt_file_exc files exc);
          acc
      ) ([], []) partition in
      (* typecheck intrinsics--temp code *)
      let forget file errs = () in
      ignore (Init_js.init forget forget forget);
      let master_cx = Flow_js.master_cx () in
      (* collect master context errors *)
      let (files, errsets) = (
        master_cx.file :: files,
        master_cx.errors :: errsets
      ) in
      (* save *)
      save_errors_or_suppressions merge_errors files errsets
  )

(* calculate module dependencies *)
let calc_dependencies files =
  let deps = List.fold_left (fun err_map file ->
    let { Module._module = m; required = reqs; _ } =
      Module.get_module_info file in
    SMap.add m (file, reqs) err_map
  ) SMap.empty files in
  let (_, partition) = Sort_js.topsort deps in
  partition

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules inferred removed =
  let errmap = Module.commit_modules inferred removed in
  save_errormap module_errors errmap

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
    (if modes.strict then (
      try
        merge_strict workers to_merge opts
      with exc ->
        prerr_endline (Printexc.to_string exc)
     ) else (
      let partition = calc_dependencies to_merge in
      merge_nonstrict partition opts
    ));
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

(* given a module M,
  - required(M) is the set of all modules required by M
  - strict_required(M) is the subset of required(M) which contribute
    items to M's signature (the name is legacy)
  - sig(M) is M plus the closure of strict_required(M), i.e.
    all modules which might contribute to M's signature, directly
    or indirectly
  Given a module M and a set of modules S,
  - M's signature depends on S if any module in sig(M) is in S
  - M depends on S if M is in S, or if the signature of any module
    in required(M) depends on S.
 *)
let rec file_depends_on memo modules f =
  let { Module._module; required; _ } = Module.get_module_info f in
  SSet.mem _module modules ||
    SSet.exists (sig_depends_on memo modules) required

and sig_depends_on memo modules m =
  match SMap.get m !memo with
  | Some b -> b
  | None ->
    let b = SSet.mem m modules || (
      Module.module_exists m && (
        let f = Module.get_file m in
        let { Module.strict_required; _ } = Module.get_module_info f in
        memo := SMap.add m false !memo;
        SSet.exists (sig_depends_on memo modules) strict_required
      )
    ) in
    memo := SMap.add m b !memo;
    b

(* The following computation is likely inefficient; it tries to narrow down
   a potentially large set of unmodified files to a potentially small set of
   files that are affected by a modification, rather than searching in the
   reverse direction, which might involve smaller sets at each step. *)

(* Files that must be rechecked include those that immediately or recursively
   depend on modules that were added, deleted, or modified as a consequence of
   the files that were directly added, deleted, or modified. In general, the map
   from files to modules may not be preserved: deleting files may delete
   modules, adding files may add modules, and modifying files may do both.
*)
let deps unmodified inferred_files removed_modules =
  (* touched modules are all that were inferred, re-inferred or removed *)
  let touched_modules = SSet.fold (fun file mods ->
    SSet.add (Module.get_module_name file) mods
  ) inferred_files removed_modules in
  (* now, add any untouched files that depend on these *)
  let memo = ref SMap.empty in
  SSet.filter (file_depends_on memo touched_modules) unmodified

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
  save_errors_or_suppressions parse_errors freshparse_fail freshparse_errors;

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
      let unmod_deps = deps unmodified_parsed inferred_set removed_modules in

      let n = SSet.cardinal unmod_deps in
        if n > 0
        then prerr_endlinef "remerge %d dependent files:" n;

      let _ = SSet.fold (fun f i ->
        prerr_endlinef "%d/%d: %s" i n f; i + 1) unmod_deps 1 in

      true, SSet.elements (SSet.union unmod_deps inferred_set)
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
  Gc.set { (Gc.get ()) with
   (*  Gc.verbose = 0x01; *)
    Gc.minor_heap_size = 64_000_000
  };
  init_modes opts;

  let parsed, error_files, errors =
    Parsing_service_js.parse workers parse_next
      (fun () -> init_modes opts)
  in
  save_errors_or_suppressions parse_errors error_files errors;

  let files = SSet.elements parsed in
  let checked = typecheck workers files SSet.empty error_files opts (
    fun inferred ->
      (* after local inference and before merge, bring in libraries *)
      (* if any fail to parse, our return value will suppress merge *)
      let lib_files = Init_js.init
        (fun file errs -> save_errors_or_suppressions parse_errors [file] [errs])
        (fun file errs -> save_errors_or_suppressions infer_errors [file] [errs])
        (fun file sups ->
          save_errors_or_suppressions error_suppressions [file] [sups])
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
  then Errors_js.print_errorl true errors stdout
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
      save_errors_or_suppressions infer_errors [package] [error]
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
