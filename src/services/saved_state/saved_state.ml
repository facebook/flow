(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type denormalized_file_data = {
  resolved_requires: Module_heaps.resolved_requires;
  exports: Exports.t;
  hash: Xx.hash;
}

type normalized_file_data = denormalized_file_data

(* For each parsed file, this is what we will save *)
type parsed_file_data = {
  info: Module_heaps.info;
  normalized_file_data: normalized_file_data;
  (* Right now there is no guarantee that this is Some, for two reasons:
   * - We allow saved state to be saved from a lazy server, meaning that it's possible that *no*
   *   files will have been merged, and therefore none will have a sig hash.
   * - We do not typecheck all parsed files, so even on a full init some files may have None here.
   *
   * The sig hashes drive optimizations, so whether or not they are included for any given file
   * should not affect correctness.
   *
   * The landscape around merging and sig hashing will change dramatically with types-first 2.0, so
   * I think that it makes sense to wait for it before deciding upon any stronger invariant to
   * enforce here.
   *)
  sig_hash: Xx.hash option;
}

(* We also need to store the info for unparsed files *)
type unparsed_file_data = {
  unparsed_info: Module_heaps.info;
  unparsed_hash: Xx.hash;
}

type saved_state_dependency_graph =
  (Utils_js.FilenameSet.t * Utils_js.FilenameSet.t) Utils_js.FilenameMap.t

(* This is the complete saved state data representation *)
type saved_state_data = {
  (* The version header should guarantee that a saved state is used by the same version of Flow.
   * However, config might have changed in a way that invalidates the saved state. In the future,
   * we probably could allow some config options, whitespace, etc. But for now, let's
   * invalidate the saved state if the config has changed at all *)
  flowconfig_hash: Xx.hash;
  parsed_heaps: (File_key.t * parsed_file_data) list;
  unparsed_heaps: (File_key.t * unparsed_file_data) list;
  (* package.json info *)
  package_heaps: (Package_json.t, unit) result FilenameMap.t;
  ordered_non_flowlib_libs: string list;
  (* Why store local errors and not merge_errors/suppressions/etc? Well, I have a few reasons:
   *
   * 1. Much smaller data structure. The whole env.errors data structure can be hundreds of MBs
   *    when marshal'd, even when there are 0 errors reported to the user)
   * 2. Saved state is designed to help skip parsing. One of the outputs of parsing are local errors
   * 3. Local errors should be the same after a lazy init and after a full init. This isn't true
   *    for the other members of env.errors which are filled in during typechecking
   *)
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t SMap.t;
  dependency_graph: saved_state_dependency_graph;
}

let modulename_map_fn ~f = function
  | Modulename.Filename fn -> Modulename.Filename (f fn)
  | Modulename.String _ as module_name -> module_name

let update_dependency_graph_filenames f graph =
  let update_set set = FilenameSet.map f set in
  let update_map update_value map =
    FilenameMap.fold
      (fun key value new_map ->
        let key = f key in
        let value = update_value value in
        FilenameMap.update
          key
          (function
            | None -> Some value
            | Some _ -> invalid_arg "Duplicate keys created by mapper function")
          new_map)
      map
      FilenameMap.empty
  in
  let update_value (sig_deps, impl_deps) =
    let sig_deps = update_set sig_deps in
    let impl_deps = update_set impl_deps in
    (sig_deps, impl_deps)
  in
  update_map update_value graph

(* It's simplest if the build ID is always the same length. Let's use 16, since that happens to
 * be the size of the build ID hash. *)
let saved_state_version_length = 16

let saved_state_version () =
  let version =
    if Build_mode.dev then
      Flow_build_id.get_build_id ()
    else
      let unpadded = Flow_version.version in
      assert (String.length unpadded <= saved_state_version_length);
      (* We have to pad out the build ID to bring it up to the right length *)
      let padding = String.make (saved_state_version_length - String.length unpadded) 'n' in
      unpadded ^ padding
  in
  assert (String.length version = saved_state_version_length);
  version

let with_cache tbl key f =
  match Hashtbl.find_opt tbl key with
  | Some result -> result
  | None ->
    let result = f key in
    Hashtbl.add tbl key result;
    result

(* Saving the saved state generally consists of 3 things:
 *
 * 1. Collecting the various bits of data
 * 2. Normalizing the data so it can be used by other Flow servers. This generally means turning
 *    absolute paths into relative paths
 * 3. Writing the data to the saved state file
 *
 * We care a little bit about the perf of generating saved state, so that any script which
 * generates saved states has an easier time keeping up. But the perf of saving isn't as important
 * as the perf of loading
 *)
module Save : sig
  val save :
    saved_state_filename:Path.t ->
    genv:ServerEnv.genv ->
    env:ServerEnv.env ->
    profiling:Profiling_js.running ->
    unit Lwt.t
end = struct
  module FileNormalizer : sig
    type t

    val make : root:string -> t

    val normalize_path : t -> string -> string

    val normalize_file_key : t -> File_key.t -> File_key.t
  end = struct
    type t = {
      root: string;
      file_key_cache: (File_key.t, File_key.t) Hashtbl.t;
    }

    let make ~root = { root; file_key_cache = Hashtbl.create 16 }

    (* We could also add a cache for this call, to improve sharing of the underlying strings
     * between file keys and the places that deal with raw paths. Unfortunately, an April 2020 test
     * of the saved state size of Facebook's largest JS codebase showed that while adding this
     * cache decreased the pre-compression size, it actually increased the post-compression size.
     * *)
    let normalize_path { root; _ } path = Files.relative_path root path

    let normalize_file_key ({ file_key_cache; _ } as normalizer) file_key =
      with_cache file_key_cache file_key (File_key.map (normalize_path normalizer))
  end

  let normalize_dependency_graph ~normalizer =
    update_dependency_graph_filenames (FileNormalizer.normalize_file_key normalizer)

  (* A Flow_error.t is a complicated data structure with Loc.t's hidden everywhere. *)
  let normalize_error ~normalizer =
    Flow_error.map_loc_of_error
      (ALoc.update_source (Base.Option.map ~f:(FileNormalizer.normalize_file_key normalizer)))

  (* We write the Flow version at the beginning of each saved state file. It's an easy way to assert
   * upon reading the file that the writer and reader are the same version of Flow *)
  let write_version fd =
    let version = saved_state_version () in

    let rec loop offset len =
      if len > 0 then
        let%lwt bytes_written = Lwt_unix.write_string fd version offset len in
        let offset = offset + bytes_written in
        let len = len - bytes_written in
        loop offset len
      else
        Lwt.return saved_state_version_length
    in
    loop 0 saved_state_version_length

  let normalize_info ~normalizer info =
    let module_name =
      modulename_map_fn
        ~f:(FileNormalizer.normalize_file_key normalizer)
        info.Module_heaps.module_name
    in
    { info with Module_heaps.module_name }

  let normalize_resolved_requires
      ~normalizer { Module_heaps.resolved_modules; phantom_dependents; hash } =
    let phantom_dependents =
      SSet.map (FileNormalizer.normalize_path normalizer) phantom_dependents
    in
    let resolved_modules =
      SMap.map
        (modulename_map_fn ~f:(FileNormalizer.normalize_file_key normalizer))
        resolved_modules
    in
    { Module_heaps.resolved_modules; phantom_dependents; hash }

  let normalize_file_data ~normalizer { resolved_requires; exports; hash } =
    let resolved_requires = normalize_resolved_requires ~normalizer resolved_requires in
    { resolved_requires; exports; hash }

  let normalize_parsed_data ~normalizer { info; normalized_file_data; sig_hash } =
    let info = normalize_info ~normalizer info in
    let normalized_file_data = normalize_file_data ~normalizer normalized_file_data in
    { info; normalized_file_data; sig_hash }

  (* Collect all the data for a single parsed file *)
  let collect_normalized_data_for_parsed_file ~normalizer ~reader fn parsed_heaps =
    let file_data =
      {
        info = Module_heaps.Reader.get_info_unsafe ~reader ~audit:Expensive.ok fn;
        normalized_file_data =
          {
            resolved_requires =
              Module_heaps.Reader.get_resolved_requires_unsafe ~reader ~audit:Expensive.ok fn;
            exports = Parsing_heaps.Reader.get_exports_unsafe ~reader fn;
            hash = Parsing_heaps.Reader.get_file_hash_unsafe ~reader fn;
          };
        sig_hash = Context_heaps.Reader.sig_hash_opt ~reader fn;
      }
    in
    let relative_fn = FileNormalizer.normalize_file_key normalizer fn in
    let relative_file_data = normalize_parsed_data ~normalizer file_data in
    (relative_fn, relative_file_data) :: parsed_heaps

  let collect_normalized_data_for_package_json_file ~normalizer fn package_heaps =
    let str = File_key.to_string fn in
    let package = Package_heaps.For_saved_state.get_package_json_unsafe str in
    let relative_fn = FileNormalizer.normalize_file_key normalizer fn in
    FilenameMap.add relative_fn package package_heaps

  (* Collect all the data for a single unparsed file *)
  let collect_normalized_data_for_unparsed_file ~normalizer ~reader fn unparsed_heaps =
    let relative_file_data =
      {
        unparsed_info =
          normalize_info ~normalizer
          @@ Module_heaps.Reader.get_info_unsafe ~reader ~audit:Expensive.ok fn;
        unparsed_hash = Parsing_heaps.Reader.get_file_hash_unsafe ~reader fn;
      }
    in
    let relative_fn = FileNormalizer.normalize_file_key normalizer fn in
    (relative_fn, relative_file_data) :: unparsed_heaps

  (* The builtin flowlibs are excluded from the saved state. The server which loads the saved state
   * will extract and typecheck its own builtin flowlibs *)
  let is_not_in_flowlib ~options =
    match (Options.file_options options).Files.default_lib_dir with
    | None -> (fun _ -> true) (* There are no flowlibs *)
    | Some root ->
      let root_str = Path.to_string root in
      (fun f -> not (Files.is_prefix root_str f))

  let normalize_error_set ~normalizer = Flow_error.ErrorSet.map (normalize_error ~normalizer)

  (* Collect all the data for all the files *)
  let collect_data ~genv ~env ~profiling =
    let options = genv.ServerEnv.options in
    let reader = State_reader.create () in
    let normalizer =
      let root = Options.root options |> Path.to_string in
      FileNormalizer.make ~root
    in
    let parsed_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectParsed" ~f:(fun () ->
          FilenameSet.fold
            (collect_normalized_data_for_parsed_file ~normalizer ~reader)
            env.ServerEnv.files
            [])
    in
    let unparsed_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectUnparsed" ~f:(fun () ->
          FilenameSet.fold
            (collect_normalized_data_for_unparsed_file ~normalizer ~reader)
            env.ServerEnv.unparsed
            [])
    in
    let package_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectPackageJson" ~f:(fun () ->
          Base.List.fold
            ~f:(fun m fn -> collect_normalized_data_for_package_json_file ~normalizer fn m)
            env.ServerEnv.package_json_files
            ~init:FilenameMap.empty)
    in
    let ordered_non_flowlib_libs =
      env.ServerEnv.ordered_libs
      |> List.filter (is_not_in_flowlib ~options)
      |> Base.List.map ~f:(FileNormalizer.normalize_path normalizer)
    in
    let local_errors =
      FilenameMap.fold
        (fun fn error_set acc ->
          let normalized_fn = FileNormalizer.normalize_file_key normalizer fn in
          let normalized_error_set = normalize_error_set ~normalizer error_set in
          FilenameMap.add normalized_fn normalized_error_set acc)
        env.ServerEnv.errors.ServerEnv.local_errors
        FilenameMap.empty
    in
    let warnings =
      FilenameMap.fold
        (fun fn warning_set acc ->
          let normalized_fn = FileNormalizer.normalize_file_key normalizer fn in
          let normalized_error_set = normalize_error_set ~normalizer warning_set in
          FilenameMap.add normalized_fn normalized_error_set acc)
        env.ServerEnv.errors.ServerEnv.warnings
        FilenameMap.empty
    in
    let node_modules_containers =
      SMap.fold
        (fun key value acc -> SMap.add (FileNormalizer.normalize_path normalizer key) value acc)
        !Files.node_modules_containers
        SMap.empty
    in
    let dependency_graph =
      let dependency_info = env.ServerEnv.dependency_info in
      let impl_map =
        dependency_info |> Dependency_info.implementation_dependency_graph |> FilenameGraph.to_map
      in
      let dependency_graph =
        let sig_map =
          dependency_info |> Dependency_info.sig_dependency_graph |> FilenameGraph.to_map
        in
        (* The maps should have the same entries. Enforce this by asserting that they have the
         * same size, and then by using `FilenameMap.find` below to ensure that each `impl_map`
         * entry has a corresponding `sig_map` entry. *)
        assert (FilenameMap.cardinal sig_map = FilenameMap.cardinal impl_map);
        FilenameMap.mapi
          (fun file impl_deps ->
            let sig_deps = FilenameMap.find file sig_map in
            (sig_deps, impl_deps))
          impl_map
      in

      normalize_dependency_graph ~normalizer dependency_graph
    in
    let flowconfig_hash =
      FlowConfig.get_hash
      @@ Server_files_js.config_file (Options.flowconfig_name options)
      @@ Options.root options
    in
    Lwt.return
      {
        flowconfig_hash;
        parsed_heaps;
        unparsed_heaps;
        package_heaps;
        ordered_non_flowlib_libs;
        local_errors;
        warnings;
        node_modules_containers;
        dependency_graph;
      }

  let save ~saved_state_filename ~genv ~env ~profiling =
    Hh_logger.info "Collecting data for saved state";

    let%lwt data = collect_data ~genv ~env ~profiling in
    let filename = Path.to_string saved_state_filename in
    let%lwt fd = Lwt_unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    let%lwt header_bytes_written = write_version fd in
    Hh_logger.info "Compressing saved state with lz4";
    let%lwt saved_state_contents =
      Profiling_js.with_timer_lwt profiling ~timer:"Compress" ~f:(fun () ->
          Saved_state_compression.(
            let compressed = marshal_and_compress data in
            let orig_size = uncompressed_size compressed in
            let new_size = compressed_size compressed in
            Hh_logger.info
              "Compressed from %d bytes to %d bytes (%3.2f%%)"
              orig_size
              new_size
              (100. *. float_of_int new_size /. float_of_int orig_size);
            Lwt.return compressed))
    in
    Profiling_js.with_timer_lwt profiling ~timer:"Write" ~f:(fun () ->
        Hh_logger.info "Writing saved-state file at %S" filename;
        let%lwt data_bytes_written =
          Marshal_tools_lwt.to_fd_with_preamble
            fd
            (saved_state_contents : Saved_state_compression.compressed)
        in
        let%lwt () = Lwt_unix.close fd in
        let bytes_written =
          header_bytes_written + Marshal_tools_lwt.expected_preamble_size + data_bytes_written
        in
        Hh_logger.info "Finished writing %d bytes to saved-state file at %S" bytes_written filename;

        Lwt.return_unit)
end

type invalid_reason =
  | Bad_header
  | Build_mismatch
  | Changed_files
  | Failed_to_marshal
  | Failed_to_decompress
  | File_does_not_exist
  | Flowconfig_mismatch

let invalid_reason_to_string = function
  | Bad_header -> "Invalid saved state header"
  | Build_mismatch -> "Build ID of saved state does not match this binary"
  | Changed_files -> "A file change invalidated the saved state"
  | Failed_to_marshal -> "Failed to unmarshal data from saved state"
  | Failed_to_decompress -> "Failed to decompress saved state data"
  | File_does_not_exist -> "Saved state file does not exist"
  | Flowconfig_mismatch -> ".flowconfig has changed since saved state was generated"

exception Invalid_saved_state of invalid_reason

(* Loading the saved state generally consists of 2 things:
 *
 * 1. Loading the saved state from a file
 * 2. Denormalizing the data. This generally means turning relative paths into absolute paths
 *
 * This is on the critical path for starting up a server with saved state. We really care about
 * the perf
 *)
module Load : sig
  val load :
    workers:MultiWorkerLwt.worker list option ->
    saved_state_filename:Path.t ->
    options:Options.t ->
    profiling:Profiling_js.running ->
    saved_state_data Lwt.t

  val denormalize_file_data : root:string -> normalized_file_data -> denormalized_file_data
end = struct
  module FileDenormalizer : sig
    type t

    val make : root:string -> t

    val denormalize_path : t -> string -> string

    val denormalize_file_key : t -> File_key.t -> File_key.t
  end = struct
    type t = {
      root: string;
      file_key_cache: (File_key.t, File_key.t) Hashtbl.t;
    }

    let make ~root = { root; file_key_cache = Hashtbl.create 16 }

    (* This could also have its own cache, but an October 2020 experiment showed that memoizing
     * these calls does not even save a single word in the denormalized saved state object. *)
    let denormalize_path { root; _ } path = Files.absolute_path root path

    let denormalize_file_key ({ file_key_cache; _ } as denormalizer) file_key =
      with_cache file_key_cache file_key (File_key.map (denormalize_path denormalizer))
  end

  let denormalize_file_key_nocache ~root fn = File_key.map (Files.absolute_path root) fn

  let denormalize_dependency_graph ~denormalizer =
    update_dependency_graph_filenames (FileDenormalizer.denormalize_file_key denormalizer)

  let denormalize_error ~denormalizer =
    Flow_error.map_loc_of_error
      (ALoc.update_source (Base.Option.map ~f:(FileDenormalizer.denormalize_file_key denormalizer)))

  let verify_version =
    (* Flow_build_id should always be 16 bytes *)
    let rec read_version fd buf offset len =
      if len > 0 then (
        let%lwt bytes_read = Lwt_unix.read fd buf offset len in
        if bytes_read = 0 then (
          Hh_logger.error
            "Invalid saved state version header. It should be %d bytes but only read %d bytes"
            saved_state_version_length
            (saved_state_version_length - len);
          raise (Invalid_saved_state Bad_header)
        );
        let offset = offset + bytes_read in
        let len = len - bytes_read in
        read_version fd buf offset len
      ) else
        let result = Bytes.to_string buf in
        let flow_build_id = saved_state_version () in
        if result <> flow_build_id then (
          Hh_logger.error
            "Saved-state file failed version check. Expected version %S but got %S"
            flow_build_id
            result;
          raise (Invalid_saved_state Build_mismatch)
        ) else
          Lwt.return_unit
    in
    fun fd ->
      read_version fd (Bytes.create saved_state_version_length) 0 saved_state_version_length

  let denormalize_info_generic ~denormalize info =
    let module_name = modulename_map_fn ~f:denormalize info.Module_heaps.module_name in
    { info with Module_heaps.module_name }

  let denormalize_info ~denormalizer info =
    denormalize_info_generic ~denormalize:(FileDenormalizer.denormalize_file_key denormalizer) info

  let denormalize_info_nocache ~root info =
    denormalize_info_generic ~denormalize:(denormalize_file_key_nocache ~root) info

  let denormalize_resolved_requires
      ~root { Module_heaps.resolved_modules; phantom_dependents; hash = _ } =
    (* We do our best to avoid reading the file system (which Path.make will do) *)
    let phantom_dependents = SSet.map (Files.absolute_path root) phantom_dependents in
    let resolved_modules =
      SMap.map (modulename_map_fn ~f:(denormalize_file_key_nocache ~root)) resolved_modules
    in
    Module_heaps.mk_resolved_requires ~resolved_modules ~phantom_dependents

  (** Turns all the relative paths in a file's data back into absolute paths. *)
  let denormalize_file_data ~root { resolved_requires; exports; hash } =
    let resolved_requires = denormalize_resolved_requires ~root resolved_requires in
    { resolved_requires; exports; hash }

  let partially_denormalize_parsed_data ~denormalizer { info; normalized_file_data; sig_hash } =
    let info = denormalize_info ~denormalizer info in
    { info; normalized_file_data; sig_hash }

  let progress_fn real_total ~total:_ ~start ~length:_ =
    MonitorRPC.status_update
      ServerStatus.(Load_saved_state_progress { total = Some real_total; finished = start })

  (* Denormalize the data for all the parsed files. This is kind of slow :( *)
  let denormalize_parsed_heaps ~denormalizer parsed_heaps =
    Base.List.map
      ~f:(fun (relative_fn, parsed_file_data) ->
        let parsed_file_data = partially_denormalize_parsed_data ~denormalizer parsed_file_data in
        let fn = FileDenormalizer.denormalize_file_key denormalizer relative_fn in
        (fn, parsed_file_data))
      parsed_heaps

  (* Denormalize the data for all the unparsed files *)
  let denormalize_unparsed_heaps ~workers ~root ~progress_fn unparsed_heaps =
    let next = MultiWorkerLwt.next ~progress_fn ~max_size:4000 workers unparsed_heaps in
    MultiWorkerLwt.call
      workers
      ~job:
        (List.fold_left (fun acc (relative_fn, unparsed_file_data) ->
             let unparsed_info = denormalize_info_nocache ~root unparsed_file_data.unparsed_info in
             let fn = denormalize_file_key_nocache ~root relative_fn in
             (fn, { unparsed_info; unparsed_hash = unparsed_file_data.unparsed_hash }) :: acc))
      ~neutral:[]
      ~merge:List.rev_append
      ~next

  let denormalize_error_set ~denormalizer =
    Flow_error.ErrorSet.map (denormalize_error ~denormalizer)

  (* Denormalize all the data *)
  let denormalize_data ~workers ~options ~data =
    let root = Options.root options |> Path.to_string in
    let denormalizer = FileDenormalizer.make ~root in
    let {
      flowconfig_hash;
      parsed_heaps;
      unparsed_heaps;
      package_heaps;
      ordered_non_flowlib_libs;
      local_errors;
      warnings;
      node_modules_containers;
      dependency_graph;
    } =
      data
    in
    let current_flowconfig_hash =
      let flowconfig_name = Options.flowconfig_name options in
      FlowConfig.get_hash @@ Server_files_js.config_file flowconfig_name @@ Options.root options
    in
    if flowconfig_hash <> current_flowconfig_hash then (
      Hh_logger.error
        "Invalid saved state: .flowconfig has changed since this saved state was generated.";
      raise (Invalid_saved_state Flowconfig_mismatch)
    );

    let package_heaps =
      FilenameMap.fold
        (fun fn package heap ->
          FilenameMap.add (FileDenormalizer.denormalize_file_key denormalizer fn) package heap)
        package_heaps
        FilenameMap.empty
    in

    Hh_logger.info "Denormalizing the data for the parsed files";
    let parsed_heaps = denormalize_parsed_heaps ~denormalizer parsed_heaps in
    Hh_logger.info "Denormalizing the data for the unparsed files";
    let%lwt unparsed_heaps =
      let progress_fn = progress_fn (List.length unparsed_heaps) in
      denormalize_unparsed_heaps ~workers ~root ~progress_fn unparsed_heaps
    in
    let ordered_non_flowlib_libs =
      Base.List.map ~f:(FileDenormalizer.denormalize_path denormalizer) ordered_non_flowlib_libs
    in
    let local_errors =
      FilenameMap.fold
        (fun normalized_fn normalized_error_set acc ->
          let fn = FileDenormalizer.denormalize_file_key denormalizer normalized_fn in
          let error_set = denormalize_error_set ~denormalizer normalized_error_set in
          FilenameMap.add fn error_set acc)
        local_errors
        FilenameMap.empty
    in
    let warnings =
      FilenameMap.fold
        (fun normalized_fn normalized_warning_set acc ->
          let fn = FileDenormalizer.denormalize_file_key denormalizer normalized_fn in
          let warning_set = denormalize_error_set ~denormalizer normalized_warning_set in
          FilenameMap.add fn warning_set acc)
        warnings
        FilenameMap.empty
    in
    let node_modules_containers =
      SMap.fold
        (fun key value acc ->
          SMap.add (FileDenormalizer.denormalize_path denormalizer key) value acc)
        node_modules_containers
        SMap.empty
    in
    let dependency_graph = denormalize_dependency_graph ~denormalizer dependency_graph in
    Lwt.return
      {
        flowconfig_hash;
        parsed_heaps;
        unparsed_heaps;
        package_heaps;
        ordered_non_flowlib_libs;
        local_errors;
        warnings;
        node_modules_containers;
        dependency_graph;
      }

  let load ~workers ~saved_state_filename ~options ~profiling =
    let filename = Path.to_string saved_state_filename in
    Hh_logger.info "Reading saved-state file at %S" filename;

    MonitorRPC.status_update ServerStatus.Read_saved_state;

    let%lwt fd =
      try%lwt Lwt_unix.openfile filename [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o666
      with Unix.Unix_error (Unix.ENOENT, _, _) as exn ->
        let exn = Exception.wrap exn in
        Hh_logger.error "Failed to open %S\n%s" filename (Exception.to_string exn);
        raise (Invalid_saved_state File_does_not_exist)
    in
    let%lwt () = verify_version fd in
    let%lwt (compressed_data : Saved_state_compression.compressed) =
      Profiling_js.with_timer_lwt profiling ~timer:"Read" ~f:(fun () ->
          try%lwt Marshal_tools_lwt.from_fd_with_preamble fd
          with exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to parse saved state data";
            raise (Invalid_saved_state Failed_to_marshal))
    in
    let%lwt () = Lwt_unix.close fd in
    Hh_logger.info "Decompressing saved-state data";

    let%lwt (data : saved_state_data) =
      Profiling_js.with_timer_lwt profiling ~timer:"Decompress" ~f:(fun () ->
          try Lwt.return (Saved_state_compression.decompress_and_unmarshal compressed_data)
          with exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to decompress saved state";
            raise (Invalid_saved_state Failed_to_decompress))
    in
    Hh_logger.info "Denormalizing saved-state data";

    let%lwt data =
      Profiling_js.with_timer_lwt profiling ~timer:"Denormalize" ~f:(fun () ->
          denormalize_data ~workers ~options ~data)
    in
    Hh_logger.info "Finished loading saved-state";

    Lwt.return data
end

let save = Save.save

let load ~workers ~saved_state_filename ~options =
  let should_print_summary = Options.should_profile options in
  Profiling_js.with_profiling_lwt ~label:"LoadSavedState" ~should_print_summary (fun profiling ->
      Load.load ~workers ~saved_state_filename ~options ~profiling)

let denormalize_file_data = Load.denormalize_file_data
