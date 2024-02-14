(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Heap = SharedMem.NewAPI
open Utils_js

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = 'a list * sig_opts_data

type 'a merge_job =
  mutator:Parsing_heaps.Merge_context_mutator.t ->
  options:Options.t ->
  for_find_all_refs:bool ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  bool * 'a

let sig_hash ~check_dirty_set ~root =
  let open Type_sig_collections in
  let open Type_sig_hash in
  let module P = Type_sig_pack in
  let module Bin = Type_sig_bin in
  let hash_file_key file_key =
    let file_string =
      match file_key with
      | File_key.LibFile path
      | File_key.SourceFile path
      | File_key.JsonFile path
      | File_key.ResourceFile path ->
        Files.relative_path (File_path.to_string root) path
    in
    Xx.hash file_string 0L
  in

  let get_type_sig_buf_unsafe file_key parse =
    match Heap.get_type_sig parse with
    | Some addr -> Heap.type_sig_buf addr
    | None -> Printf.ksprintf failwith "Expected %s to be parsed" (File_key.to_string file_key)
  in

  (* The module type of a resource dependency only depends on the file
   * extension. See Type_sig_merge.merge_resource_module_t *)
  let resource_dep f =
    let ext =
      match Utils_js.extension_of_filename f with
      | Some ext -> ext
      | None -> failwith "resource file without extension"
    in
    let hash = Xx.hash ext 0L in
    Resource (fun () -> hash)
  in

  (* A dependency which is not part of the cycle has already been merged and its
   * hashes are stored in shared memory. We can create a checked_dep record
   * containing accessors to those hashes.
   *
   * It might be useful to cache this for re-use across files in a component or
   * components in a merge batch, but this performs well enough without caching
   * for now. *)
  let acyclic_dep =
    let read_hash buf pos () = Bin.read_hash buf pos in
    let cjs_module file_key buf pos =
      let filename = Fun.const (hash_file_key file_key) in
      let info_pos = Bin.cjs_module_info buf pos in
      let (P.CJSModuleInfo
            { type_export_keys; type_stars = _; strict = _; platform_availability_set = _ }
            ) =
        Bin.read_hashed Bin.read_cjs_info buf info_pos
      in
      let type_exports = Bin.cjs_module_type_exports buf pos |> Bin.read_tbl read_hash buf in
      let exports = Bin.cjs_module_exports buf pos |> Bin.read_opt read_hash buf in
      let ns = read_hash buf info_pos in
      let type_exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
      in
      CJS { filename; type_exports; exports; ns }
    in
    let es_module file_key buf pos =
      let filename = Fun.const (hash_file_key file_key) in
      let info_pos = Bin.es_module_info buf pos in
      let (P.ESModuleInfo
            {
              type_export_keys;
              export_keys;
              type_stars = _;
              stars = _;
              strict = _;
              platform_availability_set = _;
            }
            ) =
        Bin.read_hashed Bin.read_es_info buf info_pos
      in
      let type_exports = Bin.es_module_type_exports buf pos |> Bin.read_tbl read_hash buf in
      let exports = Bin.es_module_exports buf pos |> Bin.read_tbl read_hash buf in
      let ns = read_hash buf info_pos in
      let type_exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
      in
      let exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f export_keys exports
      in
      ES { filename; type_exports; exports; ns }
    in
    fun dep_key dep_parse ->
      let buf = get_type_sig_buf_unsafe dep_key dep_parse in
      Bin.read_module_kind (cjs_module dep_key) (es_module dep_key) buf (Bin.module_kind buf)
  in

  (* Create a Type_sig_hash.checked_dep record for a file in the merged component. *)
  let cyclic_dep file_key parse file =
    let filename = Fun.const (hash_file_key file_key) in

    let type_export buf pos =
      let init_hash = Bin.read_hashed Bin.hash_serialized buf pos in
      let export = Bin.read_hashed Bin.read_type_export buf pos in
      let read_hash () = Bin.read_hash buf pos in
      let write_hash hash = Bin.write_hash buf pos hash in
      let visit edge _dep_edge = visit_type_export edge file export in
      write_hash init_hash;
      Cycle_hash.create_node visit read_hash write_hash
    in

    let cjs_exports buf pos =
      let init_hash = Bin.read_hashed Bin.hash_serialized buf pos in
      let exports = Bin.read_hashed Bin.read_packed buf pos in
      let read_hash () = Bin.read_hash buf pos in
      let write_hash hash = Bin.write_hash buf pos hash in
      let visit edge dep_edge = visit_packed edge dep_edge file exports in
      write_hash init_hash;
      Cycle_hash.create_node visit read_hash write_hash
    in

    let es_export buf pos =
      let init_hash = Bin.read_hashed Bin.hash_serialized buf pos in
      let export = Bin.read_hashed Bin.read_es_export buf pos in
      let read_hash () = Bin.read_hash buf pos in
      let write_hash hash = Bin.write_hash buf pos hash in
      let visit edge dep_edge = visit_export edge dep_edge file export in
      write_hash init_hash;
      Cycle_hash.create_node visit read_hash write_hash
    in

    let cjs_module buf pos =
      let info_pos = Bin.cjs_module_info buf pos in
      let init_hash = Bin.read_hashed Bin.hash_serialized buf info_pos in
      let (P.CJSModuleInfo
            { type_export_keys; type_stars; strict = _; platform_availability_set = _ }
            ) =
        Bin.read_hashed Bin.read_cjs_info buf info_pos
      in
      let type_exports = Bin.cjs_module_type_exports buf pos |> Bin.read_tbl type_export buf in
      let exports = Bin.cjs_module_exports buf pos |> Bin.read_opt cjs_exports buf in
      let ns =
        let visit edge dep_edge =
          Array.iter edge type_exports;
          Option.iter edge exports;
          List.iter (fun (_, index) -> edge_import_ns edge dep_edge file index) type_stars
        in
        let read_hash () = Bin.read_hash buf info_pos in
        let write_hash hash = Bin.write_hash buf info_pos hash in
        write_hash init_hash;
        Cycle_hash.create_node visit read_hash write_hash
      in
      let type_exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
      in
      CJS { filename; type_exports; exports; ns }
    in

    let es_module buf pos =
      let info_pos = Bin.es_module_info buf pos in
      let init_hash = Bin.read_hashed Bin.hash_serialized buf info_pos in
      let (P.ESModuleInfo
            {
              type_export_keys;
              export_keys;
              type_stars;
              stars;
              strict = _;
              platform_availability_set = _;
            }
            ) =
        Bin.read_hashed Bin.read_es_info buf info_pos
      in
      let type_exports = Bin.es_module_type_exports buf pos |> Bin.read_tbl type_export buf in
      let exports = Bin.es_module_exports buf pos |> Bin.read_tbl es_export buf in
      let ns =
        let visit edge dep_edge =
          Array.iter edge type_exports;
          Array.iter edge exports;
          List.iter (fun (_, index) -> edge_import_ns edge dep_edge file index) type_stars;
          List.iter (fun (_, index) -> edge_import_ns edge dep_edge file index) stars
        in
        let read_hash () = Bin.read_hash buf info_pos in
        let write_hash hash = Bin.write_hash buf info_pos hash in
        write_hash init_hash;
        Cycle_hash.create_node visit read_hash write_hash
      in
      let type_exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
      in
      let exports =
        let f acc name export = SMap.add name export acc in
        Base.Array.fold2_exn ~init:SMap.empty ~f export_keys exports
      in
      ES { filename; type_exports; exports; ns }
    in

    let buf = get_type_sig_buf_unsafe file_key parse in
    Bin.read_module_kind cjs_module es_module buf (Bin.module_kind buf)
  in

  let file_dependency ~reader component_rec component_map = function
    | Error _ -> Unchecked
    | Ok m ->
      (match Parsing_heaps.Mutator_reader.get_provider ~reader m with
      | None -> Unchecked
      | Some addr ->
        (match Parsing_heaps.read_file_key addr with
        | File_key.ResourceFile f -> resource_dep f
        | dep ->
          (match Parsing_heaps.Mutator_reader.get_typed_parse ~reader addr with
          | None -> Unchecked
          | Some parse ->
            (match FilenameMap.find_opt dep component_map with
            | Some i -> Cyclic (lazy (Lazy.force component_rec).(i))
            | None -> Acyclic (lazy (acyclic_dep dep parse))))))
  in

  (* Create a Type_sig_hash.file record for a file in the merged component. *)
  let component_file ~reader component_rec component_map (file_key, _, parse) =
    let buf = get_type_sig_buf_unsafe file_key parse in

    let dependencies =
      let resolved_modules =
        Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader Fun.id file_key parse
      in
      let f buf pos =
        let mref = Bin.read_str buf pos in
        let m = SMap.find mref resolved_modules in
        file_dependency ~reader component_rec component_map m
      in
      let pos = Bin.module_refs buf in
      Bin.read_tbl_generic f buf pos Module_refs.init
    in

    let local_defs file_rec =
      let dirty_indices =
        Bin.read_tbl_generic Bin.read_local_def_index buf (Bin.dirty_local_defs buf) Array.init
      in
      let current_index = ref 0 in
      let f buf pos =
        let def = Bin.read_local_def buf pos in
        let hash = ref (Bin.hash_serialized buf pos) in
        if check_dirty_set && Array.mem !current_index dirty_indices then hash := Int64.lognot !hash;
        let visit edge dep_edge = visit_def edge dep_edge (Lazy.force file_rec) def in
        let read_hash () = !hash in
        let write_hash = ( := ) hash in
        incr current_index;
        Cycle_hash.create_node visit read_hash write_hash
      in
      let pos = Bin.local_defs buf in
      Bin.read_tbl_generic f buf pos Local_defs.init
    in

    let remote_refs file_rec =
      let f buf pos =
        let remote_ref = Bin.read_remote_ref buf pos in
        let hash = ref (Bin.hash_serialized buf pos) in
        let visit edge dep_edge = visit_remote_ref edge dep_edge (Lazy.force file_rec) remote_ref in
        let read_hash () = !hash in
        let write_hash = ( := ) hash in
        Cycle_hash.create_node visit read_hash write_hash
      in
      let pos = Bin.remote_refs buf in
      Bin.read_tbl_generic f buf pos Remote_refs.init
    in

    let pattern_defs file_rec =
      let dirty_indices =
        Bin.read_tbl_generic Bin.read_pattern_def_index buf (Bin.dirty_pattern_defs buf) Array.init
      in
      let current_index = ref 0 in
      let f buf pos =
        let def = Bin.read_packed buf pos in
        let hash = ref (Bin.hash_serialized buf pos) in
        if check_dirty_set && Array.mem !current_index dirty_indices then hash := Int64.lognot !hash;
        let visit edge dep_edge = visit_packed edge dep_edge (Lazy.force file_rec) def in
        let read_hash () = !hash in
        let write_hash = ( := ) hash in
        incr current_index;
        Cycle_hash.create_node visit read_hash write_hash
      in
      let pos = Bin.pattern_defs buf in
      Bin.read_tbl_generic f buf pos Pattern_defs.init
    in

    let patterns file_rec =
      let f buf pos =
        let pattern = Bin.read_pattern buf pos in
        let hash = ref (Bin.hash_serialized buf pos) in
        let visit f _ = visit_pattern f (Lazy.force file_rec) pattern in
        let read_hash () = !hash in
        let write_hash = ( := ) hash in
        Cycle_hash.create_node visit read_hash write_hash
      in
      let pos = Bin.patterns buf in
      Bin.read_tbl_generic f buf pos Patterns.init
    in

    let rec file_rec =
      lazy
        {
          dependencies;
          local_defs = local_defs file_rec;
          remote_refs = remote_refs file_rec;
          pattern_defs = pattern_defs file_rec;
          patterns = patterns file_rec;
        }
    in

    cyclic_dep file_key parse (Lazy.force file_rec)
  in

  fun ~reader component ->
    (* Built a reverse lookup to detect in-cycle dependencies. *)
    let component = Array.of_list (Nel.to_list component) in
    let component_map =
      let acc = ref FilenameMap.empty in
      Array.iteri (fun i (file, _, _) -> acc := FilenameMap.add file i !acc) component;
      !acc
    in

    (* Create array of Type_sig_hash.checked_dep records, which we can use to
     * traverse the graph of signature dependencies. *)
    let rec component_rec =
      lazy (Array.map (component_file ~reader component_rec component_map) component)
    in

    (* Compute component hash by visiting graph starting at namespace root of
     * each file. The component hash is an unordered combination of each file's
     * hash. *)
    let cx = Cycle_hash.create_cx () in
    let component_hash = ref 0L in
    Array.iter
      (fun (CJS { ns; _ } | ES { ns; _ }) ->
        Cycle_hash.root cx ns;
        let file_hash = Cycle_hash.read_hash ns in
        component_hash := Int64.logxor file_hash !component_hash)
      (Lazy.force component_rec);
    !component_hash

(* Entry point for merging a component *)
let merge_component ~mutator ~options ~for_find_all_refs ~reader component =
  let start_time = Unix.gettimeofday () in
  match Parsing_heaps.Mutator_reader.typed_component ~reader component with
  | None -> (false, None)
  | Some component ->
    let hash =
      let root = Options.root options in
      sig_hash ~root ~check_dirty_set:for_find_all_refs ~reader component
    in
    let metadata = Context.metadata_of_options options in
    let lint_severities = Options.lint_severities options in
    let strict_mode = Options.strict_mode options in
    let suppressions =
      Nel.fold_left
        (fun acc (file, _, parse) ->
          let docblock = Parsing_heaps.read_docblock_unsafe file parse in
          let metadata = Context.docblock_overrides docblock file metadata in
          let lint_severities = Merge_js.get_lint_severities metadata strict_mode lint_severities in
          let (_, { Flow_ast.Program.all_comments = comments; _ }) =
            Parsing_heaps.read_ast_unsafe file parse
          in
          let (_, suppressions, _) =
            Type_inference_js.scan_for_suppressions
              ~in_libdef:false
              lint_severities
              [(file, comments)]
          in
          Error_suppressions.union suppressions acc)
        Error_suppressions.empty
        component
    in
    let diff =
      Parsing_heaps.Merge_context_mutator.add_merge_on_diff
        ~for_find_all_refs
        mutator
        component
        hash
    in
    let duration = Unix.gettimeofday () -. start_time in
    (diff, Some (suppressions, duration))

let mk_check_file options ~reader ~master_cx ~find_ref_request () =
  let check_file =
    let reader = Abstract_state_reader.Mutator_state_reader reader in
    let cache = Check_cache.create ~capacity:10000000 in
    Check_service.mk_check_file ~reader ~options ~master_cx ~cache ()
  in
  fun file ->
    let start_time = Unix.gettimeofday () in
    let addr = Parsing_heaps.get_file_addr_unsafe file in
    match Parsing_heaps.Mutator_reader.get_typed_parse ~reader addr with
    | None -> None
    | Some parse ->
      let ast = Parsing_heaps.read_ast_unsafe file parse in
      let type_sig = Parsing_heaps.read_type_sig_unsafe file parse in
      let (file_sig, tolerable_errors) = Parsing_heaps.read_tolerable_file_sig_unsafe file parse in
      let docblock = Parsing_heaps.read_docblock_unsafe file parse in
      let aloc_table = lazy (Parsing_heaps.read_aloc_table_unsafe file parse) in
      let resolved_modules =
        Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader Fun.id file parse
      in
      let (cx, typed_ast, find_ref_result) =
        check_file file resolved_modules ast file_sig docblock aloc_table find_ref_request
      in
      let coverage = Coverage.file_coverage ~cx typed_ast in
      let errors = Context.errors cx in
      let errors =
        tolerable_errors
        |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:file
        |> Flow_error.ErrorSet.union errors
      in
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
      let duration = Unix.gettimeofday () -. start_time in
      Some
        ( (cx, type_sig, file_sig, typed_ast),
          (errors, warnings, suppressions, coverage, find_ref_result, duration)
        )

(* This cache is used in check_contents_context below. When we check the
 * contents of a file, we create types from the signatures of dependencies.
 *
 * Note that this cache needs to be invaliated when files change. We can use the
 * set of changed files (determined by the merge stream's signature hashing) to
 * invalidate file-by-file when a recheck transaction commits.
 *
 * This cache also needs to be cleared when we compact the shared heap. The
 * values in this cache can contain lazy thunks which close over shared heap
 * addresses. In the event of a compaction, these addresses can become invalid.
 *
 * Any state derived from the values in this cache also needs to be reset in the
 * event of a compaction, which can be done in the SharedMem.on_compact
 * callback. *)
let check_contents_cache = Check_cache.create ~capacity:10000

(* Variation of merge_context where requires may not have already been
   resolved. This is used by commands that make up a context on the fly. *)
let check_contents_context ~reader options master_cx file ast docblock file_sig =
  (* Loading an aloc_table is unusual for check contents! During check, we use
   * this aloc table for two purposes: (1) to compare concrete and keyed alocs
   * which might be equivalent and (2) to create ALoc.id values which always
   * have the same representation for equivalent locations.
   *
   * If this file is in a cycle, an aloc table will exist and we will
   * successfully fetch it for use in cases (1) and (2). However, in the common
   * case of no cycles, an aloc table may not exist yet, which will cause an
   * exception in the (2) case. The (1) case, where a concrete and keyed
   * location are equivalent, will not occur.
   *
   * Catching the exception provides reasonable behavior, but is not the true
   * fix. Instead, if check-contents needs to deal with cycles, the cyclic
   * dependency on `file` should come from the freshly parsed type sig data, not
   * whatever data is in the heap, and the aloc table should also come from the
   * fresh parse. *)
  let aloc_table =
    lazy
      (match Parsing_heaps.Reader.get_aloc_table ~reader file with
      | Some aloc_table -> aloc_table
      | None -> ALoc.empty_table file)
  in
  let reader = Abstract_state_reader.State_reader reader in
  let resolved_modules =
    let node_modules_containers = !Files.node_modules_containers in
    let f mref = Module_js.imported_module ~options ~reader ~node_modules_containers file mref in
    SMap.mapi (fun mref _locs -> f mref) (File_sig.require_loc_map file_sig)
  in
  let check_file =
    Check_service.mk_check_file ~reader ~options ~master_cx ~cache:check_contents_cache ()
  in
  let (cx, tast, _) =
    check_file file resolved_modules ast file_sig docblock aloc_table FindRefsTypes.empty_request
  in
  (cx, tast)

(* Wrap a potentially slow operation with a timer that fires every interval seconds. When it fires,
 * it calls ~on_timer. When the operation finishes, the timer is cancelled *)
let with_async_logging_timer ~interval ~on_timer ~f =
  let start_time = Unix.gettimeofday () in
  let timer = ref None in
  let rec run_timer () = timer := Some (Timer.set_timer ~interval ~callback)
  and callback () =
    let run_time = Unix.gettimeofday () -. start_time in
    on_timer run_time;
    run_timer ()
  in
  (* Timer is unimplemented in Windows. *)
  if not Sys.win32 then run_timer ();
  let finally () = Base.Option.iter ~f:Timer.cancel_timer !timer in
  Fun.protect ~finally f

let merge_job ~mutator ~reader ~options ~for_find_all_refs ~job =
  let f acc (Merge_stream.Component ((leader, _) as component)) =
    let (diff, result) = job ~mutator ~options ~for_find_all_refs ~reader component in
    (leader, diff, result) :: acc
  in
  List.fold_left f []

let merge_runner
    ~job
    ~mutator
    ~reader
    ~options
    ~for_find_all_refs
    ~workers
    ~sig_dependency_graph
    ~components
    ~recheck_set =
  let num_workers = Options.max_workers options in
  let start_time = Unix.gettimeofday () in
  let stream = Merge_stream.create ~num_workers ~sig_dependency_graph ~components ~recheck_set in
  Merge_stream.update_server_status stream;

  (* returns parallel lists of filenames, error sets, and suppression sets *)
  let%lwt ret =
    MultiWorkerLwt.call
      workers
      ~blocking:(Options.blocking_worker_communication options)
      ~job:(merge_job ~mutator ~reader ~options ~for_find_all_refs ~job)
      ~neutral:[]
      ~merge:(Merge_stream.merge stream)
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

let mk_check options ~reader ~master_cx ~find_ref_request () =
  let check_timeout = Options.merge_timeout options in
  let slow_to_check_file_interval =
    (Options.slow_to_check_logging options).Slow_to_check_logging.slow_files_logging_internal
    |> Base.Option.value ~default:5.0
  in
  let interval =
    Base.Option.value_map
      ~f:(min slow_to_check_file_interval)
      ~default:slow_to_check_file_interval
      check_timeout
  in
  let check_file = mk_check_file options ~master_cx ~reader ~find_ref_request () in
  fun file ->
    let file_str = File_key.to_string file in
    try
      with_async_logging_timer
        ~interval
        ~on_timer:(fun run_time ->
          Hh_logger.info
            "[%d] Slow CHECK (%f seconds so far): %s"
            (Sys_utils.get_pretty_pid ())
            run_time
            file_str;
          Base.Option.iter check_timeout ~f:(fun check_timeout ->
              if run_time >= check_timeout then
                raise (Error_message.ECheckTimeout (run_time, file_str))
          ))
        ~f:(fun () -> Ok (check_file file))
    with
    | ( WorkerCancel.Worker_should_cancel | SharedMem.Out_of_shared_memory | SharedMem.Heap_full
      | SharedMem.Hash_table_full ) as exc ->
      raise exc
    (* A catch all suppression is probably a bad idea... *)
    | unwrapped_exc ->
      let exc = Exception.wrap unwrapped_exc in
      let exn_str = Printf.sprintf "%s: %s" (File_key.to_string file) (Exception.to_string exc) in
      (* In dev mode, fail hard, but log and continue in prod. *)
      if Build_mode.dev then
        Exception.reraise exc
      else
        prerr_endlinef "(%d) check_job THROWS: %s\n" (Sys_utils.get_pretty_pid ()) exn_str;
      let file_loc = Loc.{ none with source = Some file } |> ALoc.of_loc in
      (* We can't pattern match on the exception type once it's marshalled
         back to the master process, so we pattern match on it here to create
         an error result. *)
      Error
        Error_message.(
          match unwrapped_exc with
          | EDebugThrow loc -> (loc, DebugThrow)
          | ECheckTimeout (s, _) -> (file_loc, CheckTimeout s)
          | _ -> (file_loc, CheckJobException exc)
        )
