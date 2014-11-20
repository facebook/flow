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
open Constraint_js
open Modes_js

module TI = Type_inference_js
module Module = Module_js

(* per-file error info is a filename and an error list *)
type file_errors = string * Errors_js.error list

(* set of successful files, list of failed files,
 * and parallel list of error lists. *)
type results = SSet.t * string list * Errors_js.error list list

type options = {
  opt_debug : bool;
  opt_all : bool;
  opt_weak : bool;
  opt_traces : bool;
  opt_newtraces : bool;
  opt_strict : bool;
  opt_console : bool;
  opt_json : bool;
  opt_show_all_errors : bool;
  opt_quiet : bool;
  opt_profile : bool;
  opt_strip_root : bool;
  opt_module: string;
  opt_lib: string option;
}

let init_modes opts =
  modes.debug <- opts.opt_debug;
  modes.all <- opts.opt_all;
  modes.weak_by_default <- opts.opt_weak;
  modes.traces_enabled <- opts.opt_traces;
  modes.newtraces_enabled <- opts.opt_newtraces;
  modes.strict <- opts.opt_strict;
  modes.console <- opts.opt_console;
  modes.json <- opts.opt_json;
  modes.show_all_errors <- opts.opt_show_all_errors;
  modes.quiet <- opts.opt_quiet;
  modes.profile <- opts.opt_profile;
  (* TODO: confirm that only master uses strip_root, otherwise set it! *)
  Module_js.init (opts.opt_module);
  Files_js.init (opts.opt_lib)

(****************** shared context heap *********************)

(* Performance analysis suggests that we really should use WithCache instead of
   NoCache for ContextHeap (reads dominate writes). Unfortunately, we cannot do
   that yet, since we modify contexts locally after reading them from the heap.
   Proper type substitution instead of context manipulation during merge may
   eliminate this problem, one way or another.
*)

(* map from file names to contexts *)
module ContextHeap = SharedMem.NoCache (String) (struct
  type t = context
  let prefix = Prefix.make()
end)

let infer_errors = ref SMap.empty
let module_errors = ref SMap.empty
let merge_errors = ref SMap.empty
let all_errors = ref SMap.empty

let get_errors () =
  let revlist = SMap.fold (
    fun file errset ret ->
      Errors_js.ErrorSet.fold (fun flow_err ret ->
        (Errors_js.flow_error_to_hack_error flow_err) :: ret
      ) errset ret
    ) !all_errors []
  in
  List.rev revlist


(****************** typecheck job helpers *********************)

(* error state handling.
   note: once weve decoupled from hack binary, these will be stored
   in the recurrent env struct, not local state *)
let clear_errors files =
  List.iter (fun file ->
    debug_string (fun () -> spf "clear errors %s" file);
    module_errors := SMap.remove file !module_errors;
    infer_errors := SMap.remove file !infer_errors;
    merge_errors := SMap.remove file !merge_errors
  ) files

let save_errors mapref files errsets =
  List.iter2 (fun file errset ->
    mapref := SMap.add file errset !mapref
  ) files errsets

(* quick exception format *)
let fmt_exc file exc =
  file ^ ": " ^ (Printexc.to_string exc) ^ "\n"
    ^ (Printexc.get_backtrace ())

(* distribute errors from a set into a filename-indexed map,
   based on position info contained in error, not incoming key *)
let distrib_errs file eset emap =
  Errors_js.ErrorSet.fold (fun e emap ->
    let file = Errors_js.file_of_error e in
    let errs = match SMap.get file emap with
    | Some set -> Errors_js.ErrorSet.add e set
    | None -> Errors_js.ErrorSet.singleton e in
    SMap.add file errs emap
  ) eset emap

(* relocate errors to their reported positions,
   combine in single error map *)
let collate_errors workers files =
  let all = SMap.fold distrib_errs !merge_errors !infer_errors in
  let all = SMap.fold distrib_errs !module_errors all in
  all_errors := all

let wraptime opts pred msg f =
  if opts.opt_quiet || not opts.opt_profile then f () else (
    let start = Unix.gettimeofday () in
    let ret = f () in
    let elap = (Unix.gettimeofday ()) -. start in
    if not (pred elap) then () else prerr_endline (msg elap);
    ret
  )

let checktime opts limit msg f =
  wraptime opts (fun t -> t > limit) msg f

let logtime opts msg f =
  wraptime opts (fun _ -> true) msg f

(* local inference job:
   takes list of filenames, accumulates into parallel lists of
   filenames, error sets *)
let infer_job opts (inferred, errsets) files =
  init_modes opts;
  List.fold_left (fun (inferred, errsets) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: inferred %S in %f" file t)
      (fun () ->
        (*prerr_endline (spf "[%d] INFER: %s" (Unix.getpid()) file);*)

        (* infer produces a context for this module *)
        let cx = TI.infer_module file in
        (* register module info *)
        Module.add_module_info cx;
        (* note: save and clear errors before storing cx to shared heap *)
        let errs = cx.errors in
        cx.errors <- Errors_js.ErrorSet.empty;
        ContextHeap.add cx.file cx;
        (* add filename, errorset *)
        cx.file :: inferred, errs :: errsets
      )
    with exc ->
      prerr_endline (spf "(%d) infer_job THROWS: %s"
        (Unix.getpid()) (fmt_exc file exc));
      inferred, errsets
  ) (inferred, errsets) files

let rev_append_pair (x1, y1) (x2, y2) =
  (List.rev_append x1 x2, List.rev_append y1 y2)

(* local type inference pass.
   Returns a set of sucessfully inferred files.
   Creates contexts for inferred files, with errors in cx.errors *)
let infer workers files opts =
  logtime opts
    (fun t -> spf "inferred %d files in %f" (List.length files) t)
    (fun () ->
      let files, errors = MultiWorker.call
        workers
        ~job: (infer_job opts)
        ~neutral: ([], [])
        ~merge: rev_append_pair
        ~next: (Bucket.make files) in
      save_errors infer_errors files errors;
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
      Flow_js.lookup_builtin cx (spf "$module__%s" m_name)
        reason (Some (Reason_js.builtin_reason m_name)) tvar;
  ) cx.required

(* It would be nice to cache the results of merging requires. However, the naive
   scheme of letting workers add and get entries to the cache is not
   concurrency-safe: see sharedMem.mli. This means that to cache merges, we'd
   actually need to merge things in dependency order. *)

(* On the other hand, merging requires itself involves looking up a lot of
   contexts, and due to the graph-structure of dependencies, the same context
   may be looked up multiple times. To avoid redundant traffic through shared
   memory, we can cache these lookups. *)
let cached_infer_contexts = Hashtbl.create 0
let cached_infer_context file =
  try Hashtbl.find cached_infer_contexts file
  with _ ->
    let cx = ContextHeap.find_unsafe file in
    Hashtbl.replace cached_infer_contexts file cx;
    cx

(* Merging requires for a context returns a dependency graph: (a) the set of
   contexts of transitive strict requires for that context, (b) the set of edges
   between these and from these to the context. Context lookups are cached. An
   interesting property of this procedure is that it gracefully handles cycles;
   by delaying the actual substitutions, it can detect cycles via caching. *)
let rec merge_requires cx rs =
  SSet.fold (fun r (cxs,links,declarations) ->
    if Module.module_exists r then
      if checked r then
        let file = Module.get_file r in
        try
          let cx_ = Hashtbl.find cached_infer_contexts file in
          cxs,
          (cx_,cx)::links,
          declarations
        with _ ->
          let cx_ = ContextHeap.find_unsafe file in
          Hashtbl.add cached_infer_contexts file cx_;
          let (cxs_, links_, declarations_) = merge_requires cx_ cx_.strict_required in
          List.rev_append cxs_ (cx_::cxs),
          List.rev_append links_ ((cx_,cx)::links),
          List.rev_append declarations_ declarations
      else
        cxs,
        links,
        (cx,r)::declarations
    else
      cxs,
      links,
      (cx,r)::declarations
  ) rs ([],[],[])

(* To merge results for a context, check for the existence of its requires,
   compute the dependency graph (via merge_requires), and then compute
   substitutions (via merge_module_strict). A merged context is returned. *)
let merge_strict_context cx cache_function =
  if cx.checked then (
    Hashtbl.clear cached_infer_contexts;

    check_requires cx;
    cache_function ();

    let master_cx = ContextHeap.find_unsafe (Files_js.get_flowlib_root ()) in

    let cxs, links, declarations = merge_requires cx cx.required in
    TI.merge_module_strict cx cxs links declarations master_cx;
  ) else (
    (* do nothing on unchecked files *)
  );

  cx

let merge_strict_file file =
  let cx = ContextHeap.find_unsafe file in
  merge_strict_context cx (fun () -> Hashtbl.add cached_infer_contexts file cx)

let typecheck_contents contents filename autocomplete =
  match Parsing_service_js.do_parse contents filename with
  | Some ast, None ->
      let cx = TI.infer_ast ast filename "-" true in
      Some (merge_strict_context cx (fun () -> ())), cx.errors
  | _, Some errors ->
      None, errors
  | _ ->
      assert false

(* *)
let merge_strict_job opts (merged, errsets) files =
  init_modes opts;
  List.fold_left (fun (merged, errsets) file ->
    try checktime opts 1.0
      (fun t -> spf "perf: merged %S in %f" file t)
      (fun () ->
        (*prerr_endline (spf "[%d] MERGE: %s" (Unix.getpid()) file);*)

        let cx = merge_strict_file file in
        cx.file :: merged, cx.errors :: errsets
      )
    with exc ->
      prerr_endline (spf "(%d) merge_strict_job THROWS: %s\n"
        (Unix.getpid()) (fmt_exc file exc));
      (merged, errsets)
  ) (merged, errsets) files

(* *)
let merge_strict workers files opts =
  logtime opts
    (fun t -> spf "merged (strict) %d files in %f" (List.length files) t)
    (fun () ->
      (* NOTE: master_cx will only be saved once per server lifetime *)
      ContextHeap.add Flow_js.master_cx.file Flow_js.master_cx;
      (* returns parallel lists of filenames and errorsets *)
      let (files, errsets) = MultiWorker.call
        workers
        ~job: (merge_strict_job opts)
        ~neutral: ([], [])
        ~merge: rev_append_pair
        ~next: (Bucket.make files) in
      (* collect master context errors *)
      let (files, errsets) = (
        Flow_js.master_cx.file :: files,
        Flow_js.master_cx.errors :: errsets
      ) in
      (* save *)
      save_errors merge_errors files errsets
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
          prerr_endline (spf "(%d) merge_module THROWS: %s\n"
            (Unix.getpid()) (fmt_exc files exc));
          acc
      ) ([], []) partition in
      (* typecheck intrinsics--temp code *)
      Init_js.init ();
      (* collect master context errors *)
      let (files, errsets) = (
        Flow_js.master_cx.file :: files,
        Flow_js.master_cx.errors :: errsets
      ) in
      (* save *)
      save_errors merge_errors files errsets
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

(* DISABLED
   Enforce that strict dependencies don't have cycles. In the future, we
   could do something with the topsort ordering, but for now we ignore it.
*)
let enforce_strict_dependencies files =
  let deps = List.fold_left (fun err_map file ->
    let { Module._module = m; strict_required = strict_reqs; _ } =
      Module.get_module_info file in
    SMap.add m (file, strict_reqs) err_map
  ) SMap.empty files in
  let (cycle,_) = Sort_js.topsort deps in
  not cycle

(* commit newly inferred and removed modules, collect errors. *)
let commit_modules inferred removed =
  let files, errsets = Module.commit_modules inferred removed in
  save_errors module_errors files errsets

(* helper *)
(* make_merge_input takes list of files produced by infer, and
   returns list of files to merge (typically an expansion) *)
let typecheck workers files removed opts make_merge_input =
  (* local inference populates context heap, module info heap *)
  let inferred = infer workers files opts in
  (* create module dependency graph, warn on dupes etc. *)
  commit_modules inferred removed;
  (* SHUTTLE *)
  (* call supplied function to calculate closure of modules to merge *)
  let to_merge = make_merge_input inferred in
  (if modes.strict then (
    try
      (* if enforce_strict_dependencies to_merge then *)
      merge_strict workers to_merge opts
      (* else failwith "Cycle!" *)
    with exc ->
      prerr_endline (Printexc.to_string exc)
   )
  else
    let partition = calc_dependencies to_merge in
    merge_nonstrict partition opts
  );
  (* collate errors by origin *)
  collate_errors workers to_merge;
  to_merge

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

(* does `file` immediately require some module in `required_modules`? *)
let requires_any file required_modules =
  let { Module.required = req; _ } = Module.get_module_info file in
  SSet.exists (fun r -> SSet.mem r required_modules) req

(* does `file` recursively strictly-require some module in `required_modules`?
   `stack` detects cyclic strict dependencies
*)
let rec strict_requires_any file required_modules stack =
  not (SSet.mem file !stack) && (
    stack := SSet.add file !stack;
    let { Module.strict_required; _ } = Module.get_module_info file in
    strict_required |> SSet.exists (fun r ->
      transitive_mem r required_modules stack
    )
  )
and transitive_mem r required_modules stack =
  (SSet.mem r required_modules) ||
    (Module.module_exists r &&
      strict_requires_any (Module.get_file r) required_modules stack)

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
  (* now, add any that required or strict-required these *)
  SSet.filter (fun f ->
    requires_any f touched_modules
    || strict_requires_any f touched_modules (ref SSet.empty)
  ) unmodified

(* We maintain the following invariant across rechecks: The keyset of
   `files_info` contains files that parsed successfully in the previous
   phase (which could be the init phase or a previous recheck phase)
*)
let recheck genv env modified opts =
  if not opts.opt_strict
  then failwith "Missing -- strict";


  (* filter modified files *)
  let root = ServerArgs.root genv.ServerEnv.options in
  let config = FlowConfig.get root in
  let modified = SSet.filter (Files_js.wanted config) modified in

  (* clear errors for modified files and master *)
  clear_errors (Flow_js.master_cx.file :: SSet.elements modified);

  (* track deleted files, remove from modified set *)
  let deleted = SSet.filter (fun f -> not (Sys.file_exists f)) modified in
  let modified = SSet.diff modified deleted in

  (* clear errors, asts for deleted files *)
  Parsing_service_js.remove_asts deleted;

  (* reparse modified and added files *)
  let freshparsed, freshparse_fail, freshparse_errors =
    Parsing_service_js.reparse genv.ServerEnv.workers modified
      (fun () -> init_modes opts)
  in
  save_errors infer_errors freshparse_fail freshparse_errors;

  (* get old (unmodified, undeleted) files that were parsed successfully *)
  let old_parsed = Relative_path.Map.fold (fun k _ a ->
    SSet.add (Relative_path.to_absolute k) a)
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
  Flow_js.master_cx.errors <- Errors_js.ErrorSet.empty;

  (* recheck *)
  let freshparsed_list = SSet.elements freshparsed in
  typecheck genv.ServerEnv.workers freshparsed_list removed_modules opts
    (fun inferred ->
      (* need to merge the closure of inferred files and their deps *)
      let inferred_set = set_of_list inferred in
      let unmod_deps = deps unmodified_parsed inferred_set removed_modules in
      SSet.elements (SSet.union unmod_deps inferred_set)
    ) |> ignore;

  (* for now we populate file_infos with empty def lists *)
  let parsed = SSet.union freshparsed unmodified_parsed in
  let files_info = SSet.fold (fun file info ->
    let file = Relative_path.create Relative_path.Dummy file in
    Relative_path.Map.add file Parsing_service.empty_file_info info
  ) parsed Relative_path.Map.empty in

  (* NOTE: unused fields are left in their initial empty state *)
  { env with ServerEnv.files_info = files_info; }

(* full typecheck *)
let full_check workers parse_next opts =
  Gc.set { (Gc.get ()) with
   (*  Gc.verbose = 0x01; *)
    Gc.minor_heap_size = 64_000_000
  };
  init_modes opts;

  let parse_results =
    Parsing_service_js.parse workers parse_next
      (fun () -> init_modes opts)
  in
  let parsed, parse_fails, parse_errors = parse_results in
  save_errors infer_errors parse_fails parse_errors;

  let files = SSet.elements parsed in
  let checked = typecheck workers files SSet.empty opts (fun x ->
    Init_js.init ();
    x
  ) in

  (parsed, checked)

(* helper: make relative path from root to file *)
let relative_path =
  let split_path = Str.split Files_js.dir_sep in
  let rec make_relative = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) ->
        List.fold_left (fun path _ -> Filename.parent_dir_name::path) file root
  in
  fun root file ->
    make_relative (split_path root, split_path file)
    |> String.concat Filename.dir_sep

(* helper: strip root from positions *)
let strip_root p path =
  Pos.(
    let { pos_file; pos_start; pos_end } = p in
    let pos_file = Relative_path.to_absolute pos_file in
    let pos_file =
      if Files_js.is_lib_file pos_file
      then spf "[LIB] %s" (Filename.basename pos_file)
      else relative_path
        (spf "%s%s" (Path.string_of_path path) Filename.dir_sep) pos_file
    in
    {(make_from (Relative_path.create Relative_path.Dummy pos_file)) with
      pos_start; pos_end }
  )

(* helper - print errors. used in check-and-die runs *)
let print_errors ?root flow_opts =
  let errors = get_errors () in

  let errors = match root with
    | Some path ->
        let ae = Array.of_list errors in
        Array.iteri (fun i error ->
          let list = Errors.to_list error in
          let list =
            if flow_opts.opt_strip_root
            then List.map (fun (p,s) -> (strip_root p path, s)) list
            else list in
          let e = Errors.make_error list in
          ae.(i) <- e
        ) ae;
        Array.to_list ae
    | None ->
        errors
  in

  if flow_opts.opt_json
  then Errors_js.print_errorl true errors stdout
  else
    Errors_js.print_error_summary
      (not Modes_js.modes.show_all_errors)
      errors

(* initialize flow server state, including full check *)
let server_init genv env flow_opts =
  let root = ServerArgs.root genv.ServerEnv.options in
  let get_next = Files_js.make_next_files root in
  let (parsed, checked) =
    full_check genv.ServerEnv.workers get_next flow_opts in

  (* for now we populate file_infos with empty def lists *)
  let files_info = SSet.fold (fun file info ->
    let file = Relative_path.create Relative_path.Dummy file in
    Relative_path.Map.add file Parsing_service.empty_file_info info
  ) parsed Relative_path.Map.empty in

  (* We ensure an invariant required by recheck, namely that the keyset of
     `files_info` contains files that parsed successfully. *)
  (* NOTE: unused fields are left in their initial empty state *)
  let env = { env with ServerEnv.files_info = files_info; } in

  SharedMem.init_done();

  if ServerArgs.check_mode genv.ServerEnv.options
  then (
    print_errors ~root flow_opts;
    { env with ServerEnv.errorl = get_errors () }
  ) else (
    env
  )

(* single command entry point: takes a list of paths,
 * parses and checks serially, prints errs to stdout.
 *)
let single_main (paths : string list) flow_opts =
  let get_next = Files_js.make_next_files (Path.mk_path (List.hd paths)) in
  let _ = full_check None get_next flow_opts in
  print_errors flow_opts
