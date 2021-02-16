(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Loc_collections
module Reqs = Merge_js.Reqs

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type 'a file_keyed_result = File_key.t * 'a unit_result

type error_acc =
  Flow_error.ErrorSet.t
  * Flow_error.ErrorSet.t
  * Error_suppressions.t
  * Coverage_response.file_coverage FilenameMap.t option
  * float

type type_acc =
  (Context.t * File_sig.With_ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t) option

type acc = type_acc * error_acc

type 'a merge_job_results = 'a file_keyed_result list

type 'a merge_job =
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  'a unit_result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = 'a merge_job_results * sig_opts_data

type reader = Module_heaps.Reader_dispatcher.reader

(* In types-first inference is split into two phases: a "merge" and a "check" phase.
 * While these two phases differ in several aspects there are parts that are common
 * and can be abstracted into a single module (Process_unit).
 *
 * The PHASE_CONFIG interface describes the parts that are unique to each phase
 * and, as such, need to be implemented separately by "merge" and "check".
 *)
module type PHASE_CONFIG = sig
  type input

  type output

  val get_ast_unsafe : reader:reader -> File_key.t -> Merge_js.get_ast_return

  val get_file_sig_unsafe : reader:reader -> Utils_js.FilenameMap.key -> File_sig.With_ALoc.t

  val is_recursive_dep : File_key.t -> input -> bool

  val fold_input : ('acc -> File_key.t -> 'acc) -> 'acc -> input -> 'acc

  val process_with_deps :
    opts:Merge_js.merge_options ->
    getters:Merge_js.merge_getters ->
    reader:reader ->
    file_sigs:File_sig.With_ALoc.t FilenameMap.t ->
    input ->
    Reqs.t ->
    Context.sig_t list ->
    Context.sig_t ->
    output
end

module type PROCESS_UNIT = sig
  type input

  type output

  val process : options:Options.t -> reader:reader -> input -> output

  val reqs_of_input :
    reader:reader ->
    input ->
    (string * ALoc.t Nel.t * Modulename.t * File_key.t) list ->
    Context.sig_t * Context.sig_t list * Reqs.t
end

module Process_unit (C : PHASE_CONFIG) = struct
  type input = C.input

  type output = C.output

  let reqs_of_input ~reader input required =
    (* dep_cxs: the copied signature contexts of such dependencies. *)
    let (dep_cxs, reqs) =
      List.fold_left
        (fun (dep_cxs, reqs) req ->
          let (r, locs, resolved_r, file) = req in
          let locs = locs |> Nel.to_list |> ALocSet.of_list in
          Module_heaps.(
            match Reader_dispatcher.get_file ~reader ~audit:Expensive.ok resolved_r with
            | Some (File_key.ResourceFile f) -> (dep_cxs, Reqs.add_res f file locs reqs)
            | Some dep ->
              let info = Reader_dispatcher.get_info_unsafe ~reader ~audit:Expensive.ok dep in
              if info.checked && info.parsed then
                (* checked implementation exists *)
                let m = Files.module_ref dep in
                if C.is_recursive_dep dep input then
                  (* impl is part of the input *)
                  (dep_cxs, Reqs.add_impl m file locs reqs)
                else
                  (* look up impl sig_context *)
                  let leader = Context_heaps.Reader_dispatcher.find_leader ~reader dep in
                  let dep_cx = Context_heaps.Reader_dispatcher.find_sig ~reader leader in
                  (dep_cx :: dep_cxs, Reqs.add_dep_impl m file (dep_cx, locs) reqs)
              else
                (* unchecked implementation exists *)
                (dep_cxs, Reqs.add_unchecked r file locs reqs)
            | None ->
              (* implementation doesn't exist *)
              (dep_cxs, Reqs.add_decl r file (locs, resolved_r) reqs)))
        ([], Reqs.empty)
        required
    in
    let master_cx = Context_heaps.Reader_dispatcher.find_sig ~reader File_key.Builtins in
    (master_cx, dep_cxs, reqs)

  let process ~options ~reader input =
    let (required, file_sigs) =
      C.fold_input
        (fun (required, file_sigs) file ->
          let file_sig = C.get_file_sig_unsafe ~reader file in
          let file_sigs = FilenameMap.add file file_sig file_sigs in
          let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
          let required =
            SMap.fold
              (fun r locs acc ->
                let resolved_r =
                  Module_js.find_resolved_module ~reader ~audit:Expensive.ok file r
                in
                (r, locs, resolved_r, file) :: acc)
              require_loc_map
              required
          in
          (required, file_sigs))
        ([], FilenameMap.empty)
        input
    in
    let (master_cx, dep_cxs, file_reqs) = reqs_of_input ~reader input required in
    let metadata = Context.metadata_of_options options in
    let lint_severities = Options.lint_severities options in
    let strict_mode = Options.strict_mode options in
    let get_aloc_table_unsafe =
      Parsing_heaps.Reader_dispatcher.get_sig_ast_aloc_table_unsafe ~reader
    in
    let new_signatures = Options.new_signatures options in
    let opts = Merge_js.Merge_options { new_signatures; metadata; lint_severities; strict_mode } in
    let getters =
      {
        Merge_js.get_ast_unsafe = C.get_ast_unsafe ~reader;
        get_aloc_table_unsafe;
        get_docblock_unsafe = Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader;
      }
    in
    C.process_with_deps ~opts ~getters ~reader ~file_sigs input file_reqs dep_cxs master_cx
end

module Merge_config = struct
  (* The "merge" phase needs to consider _all_ files in a strongly connected component
   * at once, to handle possible recursive definitions. *)
  type input = File_key.t Nel.t

  type output = Context.t * Context.sig_t

  let get_ast_unsafe ~reader file =
    let (_, { Flow_ast.Program.all_comments; _ }) =
      Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file
    in
    let aloc_ast = Parsing_heaps.Reader_dispatcher.get_sig_ast_unsafe ~reader file in
    (all_comments, aloc_ast)

  let get_file_sig_unsafe = Parsing_heaps.Reader_dispatcher.get_sig_file_sig_unsafe

  let is_recursive_dep dep component = Nel.mem ~equal:File_key.equal dep component

  let fold_input = Nel.fold_left

  let process_with_deps ~opts ~getters ~reader:_ ~file_sigs component reqs dep_cxs master_cx =
    let ((cx, _, _), _) =
      Merge_js.merge_component ~opts ~getters ~file_sigs component reqs dep_cxs master_cx
    in
    (cx, master_cx)
end

module Merge_component :
  PROCESS_UNIT with type input = File_key.t Nel.t and type output = Merge_config.output =
  Process_unit (Merge_config)

let scan_for_component_suppressions ~options ~get_ast_unsafe component =
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  Type_sig_merge.Component.iter
    (fun file ->
      let { Type_sig_merge.key; cx; _ } = file in
      let (_, { Flow_ast.Program.all_comments; _ }) = get_ast_unsafe key in
      let metadata = Context.metadata cx in
      let lint_severities = Merge_js.get_lint_severities metadata strict_mode lint_severities in
      Type_inference_js.scan_for_suppressions cx lint_severities all_comments)
    component

let merge_context_new_signatures ~options ~reader component =
  let module Pack = Type_sig_pack in
  let module Merge = Type_sig_merge in
  let module Component = Merge.Component in
  (* make sig context, shared by all file contexts in component *)
  let sig_cx = Context.make_sig () in
  let aloc_tables =
    Nel.fold_left
      (fun tables (file : File_key.t) ->
        let table =
          lazy (Parsing_heaps.Reader_dispatcher.get_sig_ast_aloc_table_unsafe ~reader file)
        in
        FilenameMap.add file table tables)
      FilenameMap.empty
      component
  in
  let ccx = Context.make_ccx sig_cx aloc_tables in

  (* create per-file contexts *)
  let metadata = Context.metadata_of_options options in
  let create_cx file =
    let docblock = Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader file in
    let metadata = Context.docblock_overrides docblock metadata in
    let rev_table =
      let table = FilenameMap.find file aloc_tables in
      lazy
        (try Lazy.force table |> ALoc.reverse_table
         with
         (* If we aren't in abstract locations mode, or are in a libdef, we
            won't have an aloc table, so we just create an empty reverse
            table. We handle this exception here rather than explicitly
            making an optional version of the get_aloc_table function for
            simplicity. *)
         | Parsing_heaps_exceptions.Sig_ast_ALoc_table_not_found _ ->
           ALoc.make_empty_reverse_table ())
    in
    let module_ref = Files.module_ref file in
    Context.make ccx metadata file rev_table module_ref Context.Merging
  in

  (* build a reverse lookup, used to detect in-cycle dependencies *)
  let component_map = ref FilenameMap.empty in
  let component =
    Component.make component (fun i file ->
        component_map := FilenameMap.add file i !component_map;
        file)
  in
  let component_map = !component_map in

  (* dependencies *)
  let get_leader =
    let cache = Hashtbl.create 0 in
    fun file ->
      match Hashtbl.find_opt cache file with
      | Some leader -> leader
      | None ->
        let leader = Context_heaps.Reader_dispatcher.find_leader ~reader file in
        Hashtbl.add cache file leader;
        leader
  in
  let get_dep_cx =
    let cache = Hashtbl.create 0 in
    fun file ->
      let leader = get_leader file in
      match Hashtbl.find_opt cache leader with
      | Some dep_sig -> dep_sig
      | None ->
        let dep_sig = Context_heaps.Reader_dispatcher.find_sig ~reader leader in
        Context.merge_into sig_cx dep_sig;
        Hashtbl.add cache leader dep_sig;
        dep_sig
  in
  let find_dependency file mref =
    let open Module_heaps in
    let mname = Module_js.find_resolved_module ~reader ~audit:Expensive.ok file mref in
    let file = Reader_dispatcher.get_file ~reader ~audit:Expensive.ok mname in
    match file with
    | None -> Merge.BuiltinDep (mref, Modulename.to_string mname)
    | Some (File_key.ResourceFile fn) -> Merge.ResourceDep fn
    | Some dep ->
      let info = Reader_dispatcher.get_info_unsafe ~reader ~audit:Expensive.ok dep in
      if info.checked && info.parsed then
        match FilenameMap.find_opt dep component_map with
        | Some i -> Merge.CyclicDep i
        | None ->
          let dep_cx = get_dep_cx dep in
          let dep_exports = Context.find_module_sig dep_cx (Files.module_ref dep) in
          Merge.AcyclicDep dep_exports
      else
        Merge.LegacyUncheckedDepTryBuiltinsFirst mref
  in

  (* read type_sig from heap and create file record for merge *)
  let abstract_locations = Options.abstract_locations options in
  let component_file file =
    let open Type_sig_collections in
    let aloc_table = FilenameMap.find file aloc_tables in
    let aloc =
      let source = Some file in
      fun (loc : Locs.index) ->
        let aloc = ALoc.ALocRepresentationDoNotUse.make_keyed source (loc :> int) in
        if abstract_locations then
          aloc
        else
          ALoc.of_loc (ALoc.to_loc aloc_table aloc)
    in
    let {
      Packed_type_sig.Module.exports;
      export_def;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs;
      patterns;
    } =
      Parsing_heaps.Reader_dispatcher.get_type_sig_unsafe ~reader file
    in
    let dependencies =
      Module_refs.map (fun mref -> (mref, find_dependency file mref)) module_refs
    in
    let exports = Merge.create_node (Pack.map_exports aloc exports) in
    let export_def = Base.Option.map ~f:(Pack.map_packed aloc) export_def in
    let local_defs =
      Local_defs.map (fun def -> Merge.create_node (Pack.map_packed_def aloc def)) local_defs
    in
    let remote_refs =
      Remote_refs.map (fun ref -> Merge.create_node (Pack.map_remote_ref aloc ref)) remote_refs
    in
    let pattern_defs = Pattern_defs.map (Pack.map_packed aloc) pattern_defs in
    let patterns = Patterns.map (fun p -> Merge.create_node (Pack.map_pattern aloc p)) patterns in
    {
      Merge.key = file;
      cx = create_cx file;
      dependencies;
      exports;
      export_def;
      local_defs;
      remote_refs;
      pattern_defs;
      patterns;
    }
  in

  (* create component for merge, pick out leader/representative cx *)
  let component = Component.map component_file component in
  let { Merge.cx; _ } = Component.leader component in

  (* create builtins, merge master cx *)
  let master_cx = Context_heaps.Reader_dispatcher.find_sig ~reader File_key.Builtins in
  Flow_js_utils.mk_builtins cx;
  Context.merge_into sig_cx master_cx;
  Flow_js.flow_t
    cx
    ( Context.find_module_sig master_cx Files.lib_module_ref,
      Context.find_module cx Files.lib_module_ref );

  (* scan for suppressions *)
  scan_for_component_suppressions
    ~options
    ~get_ast_unsafe:(Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader)
    component;

  (* merge *)
  Merge.merge_component component;

  (cx, master_cx)

let merge_context ~options ~reader component =
  if Options.new_signatures options then
    merge_context_new_signatures ~options ~reader component
  else
    Merge_component.process ~options ~reader component

(* Entry point for merging a component *)
let merge_component ~worker_mutator ~options ~reader component =
  let start_merge_time = Unix.gettimeofday () in
  let file = Nel.hd component in

  (* We choose file as the leader, and other_files are followers. It is always
     OK to choose file as leader, as explained below.

     Note that cycles cannot happen between unchecked files. Why? Because files
     in cycles must have their dependencies recorded, yet dependencies are never
     recorded for unchecked files.

     It follows that when file is unchecked, there are no other_files! We don't
     have to worry that some other_file may be checked when file is unchecked.

     It also follows when file is checked, other_files must be checked too!
  *)
  let info = Module_heaps.Mutator_reader.get_info_unsafe ~reader ~audit:Expensive.ok file in
  if info.Module_heaps.checked then (
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let (cx, master_cx) = merge_context ~options ~reader component in
    let errors = Flow_error.ErrorSet.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let suppressions = Context.error_suppressions cx in
    let module_refs = List.rev_map Files.module_ref (Nel.to_list component) in
    let md5 = Merge_js.ContextOptimizer.sig_context cx module_refs in
    Context.clear_master_shared cx master_cx;
    Context_heaps.Merge_context_mutator.add_merge_on_diff
      ~audit:Expensive.ok
      worker_mutator
      cx
      component
      md5;
    let merge_time = Unix.gettimeofday () -. start_merge_time in
    let coverage = None in
    Ok (errors, warnings, suppressions, coverage, merge_time)
  ) else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = None in
    Ok (errors, warnings, suppressions, coverage, 0.0)

module Check_config = struct
  (* The "check" phase only needs to consider _one file_ at a time. *)
  type input = File_key.t

  type output = {
    cx: Context.t;
    master_cx: Context.sig_t;
    file_sig: File_sig.With_ALoc.t;
    typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
    coverage: Coverage_response.file_coverage;
  }

  let get_ast_unsafe ~reader file =
    let ((_, { Flow_ast.Program.all_comments; _ }) as ast) =
      Parsing_heaps.Reader_dispatcher.get_ast_unsafe ~reader file
    in
    let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
    (all_comments, aloc_ast)

  let get_file_sig_unsafe ~reader file =
    Parsing_heaps.Reader_dispatcher.get_file_sig_unsafe ~reader file |> File_sig.abstractify_locs

  let fold_input f = f

  let is_recursive_dep _ _ = false

  let process_with_deps ~opts ~getters ~reader ~file_sigs file reqs dep_cxs master_cx =
    let (cx, _, typed_ast) =
      Merge_js.check_file ~opts ~getters ~file_sigs file reqs dep_cxs master_cx
    in
    let file_sig = get_file_sig_unsafe ~reader file in
    let coverage = Coverage.file_coverage ~full_cx:cx typed_ast in
    { cx; master_cx; file_sig; typed_ast; coverage }
end

module Check_file :
  PROCESS_UNIT with type input = File_key.t and type output = Check_config.output =
  Process_unit (Check_config)

let check_file options ~reader file =
  let start_check_time = Unix.gettimeofday () in
  let info = Module_heaps.Mutator_reader.get_info_unsafe ~reader ~audit:Expensive.ok file in
  if info.Module_heaps.checked then
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let { Check_config.cx; coverage; typed_ast; file_sig; _ } =
      Check_file.process ~options ~reader file
    in
    let errors = Context.errors cx in
    let suppressions = Context.error_suppressions cx in
    let severity_cover = Context.severity_cover cx in
    let include_suppressions = Context.include_suppressions cx in
    let aloc_tables = Context.aloc_tables cx in
    let (errors, warnings, suppressions) =
      Error_suppressions.filter_lints
        ~include_suppressions
        suppressions
        errors
        aloc_tables
        severity_cover
    in
    let coverage = Some (FilenameMap.singleton file coverage) in
    let check_time = Unix.gettimeofday () -. start_check_time in
    (Some (cx, file_sig, typed_ast), (errors, warnings, suppressions, coverage, check_time))
  else
    let errors = Flow_error.ErrorSet.empty in
    let suppressions = Error_suppressions.empty in
    let warnings = Flow_error.ErrorSet.empty in
    let coverage = None in
    (None, (errors, warnings, suppressions, coverage, 0.0))

(* Variation of merge_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let check_contents_context ~reader options file ast info file_sig =
  let (_, { Flow_ast.Program.all_comments; _ }) = ast in
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let reader = Abstract_state_reader.State_reader reader in
  let file_sig = File_sig.abstractify_locs file_sig in
  let required =
    let require_loc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
    SMap.fold
      (fun r (locs : ALoc.t Nel.t) required ->
        let resolved_r =
          Module_js.imported_module
            ~options
            ~reader
            ~node_modules_containers:!Files.node_modules_containers
            file
            (Nel.hd locs)
            r
        in
        (r, locs, resolved_r, file) :: required)
      require_loc_map
      []
  in
  let file_sigs = FilenameMap.singleton file file_sig in
  let (master_cx, dep_cxs, file_reqs) =
    try Check_file.reqs_of_input ~reader file required with
    | Key_not_found _ -> failwith "not all dependencies are ready yet, aborting..."
    | e -> raise e
  in
  let metadata = Context.metadata_of_options options in
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  let get_aloc_table_unsafe =
    Parsing_heaps.Reader_dispatcher.get_sig_ast_aloc_table_unsafe ~reader
  in
  let new_signatures = Options.new_signatures options in
  let opts = Merge_js.Merge_options { new_signatures; metadata; lint_severities; strict_mode } in
  let getters =
    {
      Merge_js.get_ast_unsafe = (fun _ -> (all_comments, aloc_ast));
      get_aloc_table_unsafe;
      get_docblock_unsafe = (fun _ -> info);
    }
  in
  let (cx, _, tast) =
    Merge_js.check_file ~opts ~getters ~file_sigs file file_reqs dep_cxs master_cx
  in
  (cx, tast)

(* Wrap a potentially slow operation with a timer that fires every interval seconds. When it fires,
 * it calls ~on_timer. When the operation finishes, the timer is cancelled *)
let with_async_logging_timer ~interval ~on_timer ~f =
  let start_time = Unix.gettimeofday () in
  let timer = ref None in
  let cancel_timer () = Base.Option.iter ~f:Timer.cancel_timer !timer in
  let rec run_timer ?(first_run = false) () =
    ( if not first_run then
      let run_time = Unix.gettimeofday () -. start_time in
      on_timer run_time );
    timer := Some (Timer.set_timer ~interval ~callback:run_timer)
  in
  (* Timer is unimplemented in Windows. *)
  if not Sys.win32 then run_timer ~first_run:true ();
  let ret =
    try f ()
    with e ->
      cancel_timer ();
      raise e
  in
  cancel_timer ();
  ret

let merge_job ~worker_mutator ~reader ~job ~options merged elements =
  List.fold_left
    (fun merged -> function
      | Merge_stream.Component component ->
        (* A component may have several files: there's always at least one, and
           multiple files indicate a cycle. *)
        let files =
          component |> Nel.to_list |> Base.List.map ~f:File_key.to_string |> String.concat "\n\t"
        in
        let merge_timeout = Options.merge_timeout options in
        let interval = Base.Option.value_map ~f:(min 15.0) ~default:15.0 merge_timeout in
        (try
           with_async_logging_timer
             ~interval
             ~on_timer:(fun run_time ->
               Hh_logger.info
                 "[%d] Slow MERGE (%f seconds so far): %s"
                 (Unix.getpid ())
                 run_time
                 files;
               Base.Option.iter merge_timeout ~f:(fun merge_timeout ->
                   if run_time >= merge_timeout then
                     raise (Error_message.EMergeTimeout (run_time, files))))
             ~f:(fun () ->
               let start_time = Unix.gettimeofday () in
               (* prerr_endlinef "[%d] MERGE: %s" (Unix.getpid()) files; *)
               let ret = job ~worker_mutator ~options ~reader component in
               let merge_time = Unix.gettimeofday () -. start_time in
               if Options.should_profile options then (
                 let length = Nel.length component in
                 let leader = Nel.hd component |> File_key.to_string in
                 Flow_server_profile.merge ~length ~merge_time ~leader;
                 if merge_time > 1.0 then
                   Hh_logger.info "[%d] perf: merged %s in %f" (Unix.getpid ()) files merge_time
               );
               (Nel.hd component, ret) :: merged)
         with
        | (SharedMem.Out_of_shared_memory | SharedMem.Heap_full | SharedMem.Hash_table_full) as exc
          ->
          raise exc
        (* A catch all suppression is probably a bad idea... *)
        | unwrapped_exc ->
          let exc = Exception.wrap unwrapped_exc in
          let exn_str = Printf.sprintf "%s: %s" files (Exception.to_string exc) in
          (* Ensure heaps are in a good state before continuing. *)
          Context_heaps.Merge_context_mutator.add_merge_on_exn
            ~audit:Expensive.ok
            worker_mutator
            ~options
            component;

          (* In dev mode, fail hard, but log and continue in prod. *)
          if Build_mode.dev then
            Exception.reraise exc
          else
            prerr_endlinef
              "(%d) merge_job THROWS: [%d] %s\n"
              (Unix.getpid ())
              (Nel.length component)
              exn_str;

          (* An errored component is always changed. *)
          let file = Nel.hd component in
          let file_loc = Loc.{ none with source = Some file } |> ALoc.of_loc in
          (* We can't pattern match on the exception type once it's marshalled
             back to the master process, so we pattern match on it here to create
             an error result. *)
          let result =
            Error
              Error_message.(
                match unwrapped_exc with
                | EDebugThrow loc -> (loc, DebugThrow)
                | EMergeTimeout (s, _) -> (file_loc, MergeTimeout s)
                | _ -> (file_loc, MergeJobException exc))
          in
          (file, result) :: merged))
    merged
    elements

let merge_runner
    ~job
    ~master_mutator
    ~worker_mutator
    ~reader
    ~intermediate_result_callback
    ~options
    ~workers
    ~sig_dependency_graph
    ~component_map
    ~recheck_set =
  let num_workers = Options.max_workers options in
  (* (1) make a map from files to their component leaders
     (2) lift recheck set from files to their component leaders *)
  let (leader_map, recheck_leader_set) =
    FilenameMap.fold
      (fun leader component (leader_map, recheck_leader_set) ->
        let (leader_map, recheck_leader) =
          Nel.fold_left
            (fun (leader_map, recheck_leader) file ->
              ( FilenameMap.add file leader leader_map,
                recheck_leader || FilenameSet.mem file recheck_set ))
            (leader_map, false)
            component
        in
        let recheck_leader_set =
          if recheck_leader then
            FilenameSet.add leader recheck_leader_set
          else
            recheck_leader_set
        in
        (leader_map, recheck_leader_set))
      component_map
      (FilenameMap.empty, FilenameSet.empty)
  in
  let start_time = Unix.gettimeofday () in
  let stream =
    Merge_stream.create
      ~num_workers
      ~sig_dependency_graph
      ~leader_map
      ~component_map
      ~recheck_leader_set
      ~intermediate_result_callback
  in
  Merge_stream.update_server_status stream;

  (* returns parallel lists of filenames, error sets, and suppression sets *)
  let%lwt ret =
    MultiWorkerLwt.call
      workers
      ~job:(merge_job ~worker_mutator ~reader ~options ~job)
      ~neutral:[]
      ~merge:(Merge_stream.merge ~master_mutator ~reader stream)
      ~next:(Merge_stream.next stream)
  in
  let total_files = Merge_stream.total_files stream in
  let skipped_count = Merge_stream.skipped_count stream in
  let sig_new_or_changed = Merge_stream.sig_new_or_changed stream in
  Hh_logger.info "Merge skipped %d of %d modules" skipped_count total_files;
  let elapsed = Unix.gettimeofday () -. start_time in
  if Options.should_profile options then Hh_logger.info "merged in %f" elapsed;
  Lwt.return (ret, { skipped_count; sig_new_or_changed })

let merge = merge_runner ~job:merge_component

let check options ~reader file =
  let result =
    let check_timeout = Options.merge_timeout options in
    (* TODO: add new option *)
    let interval = Base.Option.value_map ~f:(min 5.0) ~default:5.0 check_timeout in
    let file_str = File_key.to_string file in
    try
      with_async_logging_timer
        ~interval
        ~on_timer:(fun run_time ->
          Hh_logger.info
            "[%d] Slow CHECK (%f seconds so far): %s"
            (Unix.getpid ())
            run_time
            file_str;
          Base.Option.iter check_timeout ~f:(fun check_timeout ->
              if run_time >= check_timeout then
                raise (Error_message.ECheckTimeout (run_time, file_str))))
        ~f:(fun () -> Ok (check_file options ~reader file))
    with
    | (SharedMem.Out_of_shared_memory | SharedMem.Heap_full | SharedMem.Hash_table_full) as exc ->
      raise exc
    (* A catch all suppression is probably a bad idea... *)
    | unwrapped_exc ->
      let exc = Exception.wrap unwrapped_exc in
      let exn_str = Printf.sprintf "%s: %s" (File_key.to_string file) (Exception.to_string exc) in
      (* In dev mode, fail hard, but log and continue in prod. *)
      if Build_mode.dev then
        Exception.reraise exc
      else
        prerr_endlinef "(%d) check_job THROWS: %s\n" (Unix.getpid ()) exn_str;
      let file_loc = Loc.{ none with source = Some file } |> ALoc.of_loc in
      (* We can't pattern match on the exception type once it's marshalled
         back to the master process, so we pattern match on it here to create
         an error result. *)
      Error
        Error_message.(
          match unwrapped_exc with
          | EDebugThrow loc -> (loc, DebugThrow)
          | ECheckTimeout (s, _) -> (file_loc, CheckTimeout s)
          | _ -> (file_loc, CheckJobException exc))
  in
  (file, result)
