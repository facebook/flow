(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* # Saving and loading saved states:
 *
 * ## Relative / absolute paths
 *
 * Flow stores absolute paths, but the server loading a saved state might store
 * the repo root at a different location. To handle this, we convert all
 * absolute paths to root-relative paths in the saved state, then convert those
 * back into absolute paths when we load the saved state.
 *
 * ## Dealing with balanced trees
 *
 * Flow stores many collections as balanced trees. When loading, we would like
 * to re-construct these balanced trees efficiently, avoiding comparisons and
 * rebalancing operations.
 *
 * When storing, we convert balanced trees to arrays, ensuring that the arrays
 * are sorted with respect to the keys. During load, we can use the unsafe API
 * `of_array_unchecked` to efficiently build maps.
 *
 * ## Balanced trees sorted by absolute path
 *
 * Sometimes the keys of the balanced trees are absolute file names. We can
 * safely re-construct these trees, even when the repo root is in a different
 * directory.
 *
 * Consider two absolute paths, A1 and A2, so that A1 < A2. If we convert these
 * to paths R1 and R2 relative to directory D1, then convert them back into
 * absolute paths A1' and A2' relative to directory D2, then A1' < A2'.
 *
 * Caveat: if A1/A2 are outside of D1, i.e., R1/R2 paths start with `..`, then
 * the above might not hold. This is possible because [includes] can refer to
 * files that are outside the Flow root. In this case, we rely on a relaxed
 * invariant, where both D1 and D2 both have a common ancestor directory D0,
 * the same relative path w.r.t. D0, and all files are contained within D0.
 *
 * In practice, D0 is a source control root. *)

module FMap = Utils_js.FilenameMap
module FSet = Utils_js.FilenameSet
module FGraph = Utils_js.FilenameGraph
module MSet = Modulename.Set

type path = {
  relative: string;
  mutable absolute: string option;
}

type file_key =
  | Source_file of path
  | Json_file of path
  | Resource_file of path
  | Lib_file of path

type modulename =
  | Haste_module of string
  | File_module of file_key

type parse = {
  exports: Exports.t;
  resolved_modules: (string * (modulename, string) Result.t) array;
  phantom_dependencies: modulename array;
}

type aloc = (ALoc.t, file_key * ALoc.t) Either.t

(* This is the complete saved state data representation *)
type serialized_t =
  | Saved_state of {
      (* The version header should guarantee that a saved state is used by the same version of Flow.
       * However, config might have changed in a way that invalidates the saved state. In the future,
       * we probably could allow some config options, whitespace, etc. But for now, let's
       * invalidate the saved state if the config has changed at all *)
      flowconfig_hash: int64;
      parsed: (file_key * int64 * string option * parse) array;
      unparsed: (file_key * int64 * string option) array;
      package_json: (file_key * (Package_json.t, unit) result) list;
      rev_non_flowlib_libs: path list;
      (* Why store local errors and not merge_errors/suppressions/etc? Well, I have a few reasons:
       *
       * 1. Much smaller data structure. The whole env.errors data structure can be hundreds of MBs
       *    when marshal'd, even when there are 0 errors reported to the user)
       * 2. Saved state is designed to help skip parsing. One of the outputs of parsing are local errors
       * 3. Local errors should be the same after a lazy init and after a full init. This isn't true
       *    for the other members of env.errors which are filled in during typechecking
       *)
      local_errors: (file_key * aloc Flow_error.t array) array;
      warnings: (file_key * aloc Flow_error.t array) array;
      node_modules_containers: (path * SSet.t) array;
      dependency_graph: (file_key * file_key array * file_key array) array;
    }

type t = {
  parsed: Utils_js.FilenameSet.t;
  unparsed: Utils_js.FilenameSet.t;
  package_json_files: File_key.t list;
  node_modules_containers: SSet.t SMap.t;
  dependency_info: Dependency_info.t;
  ordered_non_flowlib_libs: string list;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  dirty_modules: Modulename.Set.t;
}

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

let get_flowconfig_hash ~options =
  let flowconfig_name = Options.flowconfig_name options in
  let root = Options.root options in
  FlowConfig.get_hash (Server_files_js.config_file flowconfig_name root)

module Save = struct
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

  let mk_env root =
    let tbl = Hashtbl.create 0 in
    let relative_path = Files.relative_path root in
    fun abs_path ->
      match Hashtbl.find_opt tbl abs_path with
      | Some path -> path
      | None ->
        let path = { relative = relative_path abs_path; absolute = None } in
        Hashtbl.add tbl abs_path path;
        path

  let get_path env = env

  let get_key env = function
    | File_key.SourceFile path -> Source_file (get_path env path)
    | File_key.JsonFile path -> Json_file (get_path env path)
    | File_key.ResourceFile path -> Resource_file (get_path env path)
    | File_key.LibFile path -> Lib_file (get_path env path)

  let get_module env = function
    | Modulename.String name -> Haste_module name
    | Modulename.Filename key -> File_module (get_key env key)

  let get_resolved_module env = function
    | Ok m -> Ok (get_module env m)
    | Error _ as err -> err

  let collect_parsed env reader file_key acc =
    let file = Parsing_heaps.get_file_addr_unsafe file_key in
    let parse = Parsing_heaps.Reader.get_typed_parse_unsafe ~reader file_key file in
    let { Parsing_heaps.resolved_modules; phantom_dependencies; _ } =
      Parsing_heaps.Reader.get_resolved_requires_unsafe file_key ~reader parse
    in
    let resolved_modules =
      SMap.fold (fun mref m acc -> (mref, get_resolved_module env m) :: acc) resolved_modules []
      |> Base.Array.of_list_rev
    in
    let phantom_dependencies =
      MSet.fold (fun m acc -> get_module env m :: acc) phantom_dependencies []
      |> Base.Array.of_list_rev
    in
    let file_key = get_key env file_key in
    let file_hash = Parsing_heaps.read_file_hash parse in
    let haste_name = Parsing_heaps.Reader.get_haste_name ~reader file in
    let parse =
      { exports = Parsing_heaps.read_exports parse; resolved_modules; phantom_dependencies }
    in
    (file_key, file_hash, haste_name, parse) :: acc

  let collect_unparsed env reader file_key acc =
    let file = Parsing_heaps.get_file_addr_unsafe file_key in
    let parse = Parsing_heaps.Reader.get_parse_unsafe ~reader file_key file in
    let file_key = get_key env file_key in
    let file_hash = Parsing_heaps.read_file_hash parse in
    let haste_name = Parsing_heaps.Reader.get_haste_name ~reader file in
    (file_key, file_hash, haste_name) :: acc

  let collect_package_json env acc file_key =
    let str = File_key.to_string file_key in
    let package = Package_heaps.For_saved_state.get_package_json_unsafe str in
    (get_key env file_key, package) :: acc

  (* The builtin flowlibs are excluded from the saved state. The server which loads the saved state
   * will extract and typecheck its own builtin flowlibs *)
  let collect_lib env options =
    let file_options = Options.file_options options in
    let is_in_flowlib = Files.is_in_flowlib file_options in
    fun acc lib ->
      if is_in_flowlib lib then
        acc
      else
        get_path env lib :: acc

  let collect_err env =
    let f loc =
      match ALoc.source loc with
      | None -> Either.Left loc
      | Some file_key ->
        let file_key = get_key env file_key in
        let loc = ALoc.update_source (Fun.const None) loc in
        Either.Right (file_key, loc)
    in
    (fun err acc -> Flow_error.map_loc_of_error f err :: acc)

  let collect_errs env file_key errs acc =
    let errs = Flow_error.ErrorSet.fold (collect_err env) errs [] |> Base.Array.of_list_rev in
    (get_key env file_key, errs) :: acc

  let collect_node_modules_containers env container node_modules acc =
    (get_path env container, node_modules) :: acc

  let collect ~options ~profiling env =
    let {
      ServerEnv.files = parsed;
      unparsed;
      package_json_files;
      ordered_libs;
      errors = { ServerEnv.local_errors; warnings; _ };
      dependency_info;
      _;
    } =
      env
    in
    let root = Options.root options in
    let env = mk_env (Path.to_string root) in
    let reader = State_reader.create () in
    let%lwt parsed =
      Profiling_js.with_timer profiling ~timer:"CollectParsed" ~f:(fun () ->
          FSet.fold (collect_parsed env reader) parsed [] |> Base.Array.of_list_rev |> Lwt.return
      )
    in
    let%lwt unparsed =
      Profiling_js.with_timer profiling ~timer:"CollectUnparsed" ~f:(fun () ->
          FSet.fold (collect_unparsed env reader) unparsed []
          |> Base.Array.of_list_rev
          |> Lwt.return
      )
    in
    let%lwt package_json =
      Profiling_js.with_timer profiling ~timer:"CollectPackageJson" ~f:(fun () ->
          List.fold_left (collect_package_json env) [] package_json_files |> Lwt.return
      )
    in
    let rev_non_flowlib_libs = List.fold_left (collect_lib env options) [] ordered_libs in
    let local_errors = FMap.fold (collect_errs env) local_errors [] |> Base.Array.of_list_rev in
    let warnings = FMap.fold (collect_errs env) warnings [] |> Base.Array.of_list_rev in
    let node_modules_containers =
      SMap.fold (collect_node_modules_containers env) !Files.node_modules_containers []
      |> Base.Array.of_list_rev
    in
    let%lwt dependency_graph =
      let f dep_key acc = get_key env dep_key :: acc in
      let f sig_map file_key impl_deps acc =
        let sig_deps = FMap.find file_key sig_map in
        let sig_deps = FSet.fold f sig_deps [] |> Base.Array.of_list_rev in
        let impl_deps = FSet.fold f impl_deps [] |> Base.Array.of_list_rev in
        (get_key env file_key, sig_deps, impl_deps) :: acc
      in
      Profiling_js.with_timer profiling ~timer:"CollectDependencyInfo" ~f:(fun () ->
          let impl_map =
            Dependency_info.implementation_dependency_graph dependency_info |> FGraph.to_map
          in
          let sig_map = Dependency_info.sig_dependency_graph dependency_info |> FGraph.to_map in
          Lwt.return (FMap.fold (f sig_map) impl_map [] |> Base.Array.of_list_rev)
      )
    in
    Lwt.return
      (Saved_state
         {
           flowconfig_hash = get_flowconfig_hash ~options;
           parsed;
           unparsed;
           package_json;
           rev_non_flowlib_libs;
           local_errors;
           warnings;
           node_modules_containers;
           dependency_graph;
         }
      )

  let save ~saved_state_filename ~options ~profiling env =
    Hh_logger.info "Collecting data for saved state";
    let%lwt saved_state = collect ~options ~profiling env in
    let filename = Path.to_string saved_state_filename in
    let%lwt fd = Lwt_unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    let%lwt header_bytes_written = write_version fd in
    let%lwt compressed =
      Profiling_js.with_timer_lwt profiling ~timer:"Compress" ~f:(fun () ->
          let open Saved_state_compression in
          Hh_logger.info "Compressing saved state with lz4";
          let compressed = marshal_and_compress saved_state in
          let orig_size = uncompressed_size compressed in
          let new_size = compressed_size compressed in
          Hh_logger.info
            "Compressed from %d bytes to %d bytes (%3.2f%%)"
            orig_size
            new_size
            (100. *. float_of_int new_size /. float_of_int orig_size);
          Lwt.return compressed
      )
    in
    Profiling_js.with_timer_lwt profiling ~timer:"Write" ~f:(fun () ->
        Hh_logger.info "Writing saved-state file at %S" filename;
        let%lwt data_bytes_written =
          Marshal_tools_lwt.to_fd_with_preamble fd (compressed : Saved_state_compression.compressed)
        in
        let%lwt () = Lwt_unix.close fd in
        let bytes_written =
          header_bytes_written + Marshal_tools_lwt.expected_preamble_size + data_bytes_written
        in
        Hh_logger.info "Finished writing %d bytes to saved-state file at %S" bytes_written filename;

        Lwt.return_unit
    )
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

module Load = struct
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

  let mk_env root =
    let absolute_path = Files.absolute_path root in
    fun path ->
      match path.absolute with
      | Some absolute_path -> absolute_path
      | None ->
        let abs_path = absolute_path path.relative in
        path.absolute <- Some abs_path;
        abs_path

  let load_path env = env

  let load_key env = function
    | Source_file path -> File_key.SourceFile (load_path env path)
    | Json_file path -> File_key.JsonFile (load_path env path)
    | Resource_file path -> File_key.ResourceFile (load_path env path)
    | Lib_file path -> File_key.LibFile (load_path env path)

  let load_module env = function
    | Haste_module name -> Modulename.String name
    | File_module key -> Modulename.Filename (load_key env key)


  let load_parse options dirty_modules env parsed =
    FSet.of_array_unchecked parsed (fun (file_key, hash, haste_name, {exports; resolved_modules; phantom_dependencies}) ->
      let file_key = load_key env file_key in
      let resolved_modules : (Modulename.t, string) result SMap.t =  
        SMap.of_array_unchecked resolved_modules 
           (fun (mref, m)  -> 
            match m with 
            | Error _ as err -> (mref,err)
            | Ok m -> (mref, Ok (load_module env m : Modulename.t))) 
      in 
      let phantom_dependencies = 
        MSet.of_array_unchecked phantom_dependencies (fun m -> load_module env m) in
      let resolved_requires =
        Parsing_heaps.mk_resolved_requires ~resolved_modules ~phantom_dependencies
      in
      let ms =
        Parsing_heaps.From_saved_state.add_parsed
          options
          file_key
          hash
          haste_name
          exports
          resolved_requires
      in
      dirty_modules := MSet.union ms !dirty_modules;
      file_key
   )

  let load_unparsed options dirty_modules env unparsed =
    FSet.of_array_unchecked unparsed (fun (file_key, hash, haste_name) ->
      let file_key = load_key env file_key in
      let ms = Parsing_heaps.From_saved_state.add_unparsed options file_key hash haste_name in
      dirty_modules := MSet.union ms !dirty_modules;
      file_key
    )

  let load_errs env errs =
    let map_err =
      let f = function
        | Either.Left loc -> loc
        | Either.Right (key, loc) ->
          let f _ = Some (load_key env key) in
          ALoc.update_source f loc
      in
      Flow_error.map_loc_of_error f
    in
    Flow_error.ErrorSet.of_array_unchecked errs map_err

  let load_file_errs env file_errs =    
    FMap.of_array_unchecked file_errs (fun (file_key,errs) -> 
      load_key env file_key, load_errs env errs
  )
  
  let load_deps env deps =
    FSet.of_array_unchecked deps (fun dep -> load_key env dep)

  let load_dependency_info env graph =
      FMap.of_array_unchecked graph (fun (file_key, sig_deps, impl_deps) -> 
        let sig_deps = load_deps env sig_deps in
        let impl_deps = load_deps env impl_deps in
        (load_key env file_key, (sig_deps, impl_deps))
        )|> Dependency_info.of_map

  let load_saved_state ~options ~profiling saved_state =
    let root = Path.to_string (Options.root options) in
    let env = mk_env root in
    let (Saved_state
          {
            flowconfig_hash;
            parsed;
            unparsed;
            package_json;
            rev_non_flowlib_libs;
            local_errors;
            warnings;
            node_modules_containers;
            dependency_graph;
          }
          ) =
      saved_state
    in

    if flowconfig_hash <> get_flowconfig_hash ~options then (
      Hh_logger.error
        "Invalid saved state: .flowconfig has changed since this saved state was generated.";
      raise (Invalid_saved_state Flowconfig_mismatch)
    );

    let dirty_modules = ref MSet.empty in

    let%lwt parsed =
      Profiling_js.with_timer_lwt profiling ~timer:"LoadParsed" ~f:(fun () ->
          Lwt.return (load_parse options dirty_modules env parsed)
      )
    in

    let%lwt unparsed =
      Profiling_js.with_timer_lwt profiling ~timer:"LoadUnparsed" ~f:(fun () ->
          Lwt.return (load_unparsed options dirty_modules env unparsed)
      )
    in

    let%lwt package_json_files =
      let f acc (file_key, package) =
        let file_key = load_key env file_key in
        Module_js.add_package (File_key.to_string file_key) package;
        file_key :: acc
      in
      Profiling_js.with_timer_lwt profiling ~timer:"LoadPackageJson" ~f:(fun () ->
          Lwt.return (List.fold_left f [] package_json)
      )
    in

    let ordered_non_flowlib_libs = List.rev_map (load_path env) rev_non_flowlib_libs in

    let node_modules_containers =
      SMap.of_array_unchecked node_modules_containers (fun (container, node_modules) ->
        (load_path env container, node_modules))
    in
    let%lwt dependency_info =
      Profiling_js.with_timer_lwt profiling ~timer:"LoadDependencyInfo" ~f:(fun () ->
          Lwt.return (load_dependency_info env dependency_graph)
      )
    in

    let local_errors = load_file_errs env local_errors in
    let warnings = load_file_errs env warnings in

    Lwt.return
      {
        parsed;
        unparsed;
        package_json_files;
        ordered_non_flowlib_libs;
        node_modules_containers;
        dependency_info;
        local_errors;
        warnings;
        dirty_modules = !dirty_modules;
      }

  let load ~saved_state_filename ~options ~profiling =
    let filename = Path.to_string saved_state_filename in
    Hh_logger.info "Reading saved-state file at %S" filename;

    MonitorRPC.status_update ~event:ServerStatus.Read_saved_state;

    let%lwt fd =
      try%lwt Lwt_unix.openfile filename [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o666 with
      | Unix.Unix_error (Unix.ENOENT, _, _) as exn ->
        let exn = Exception.wrap exn in
        Hh_logger.error "Failed to open %S\n%s" filename (Exception.to_string exn);
        raise (Invalid_saved_state File_does_not_exist)
    in
    let%lwt () = verify_version fd in
    let%lwt (compressed : Saved_state_compression.compressed) =
      Profiling_js.with_timer_lwt profiling ~timer:"Read" ~f:(fun () ->
          try%lwt Marshal_tools_lwt.from_fd_with_preamble fd with
          | exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to parse saved state data";
            raise (Invalid_saved_state Failed_to_marshal)
      )
    in
    let%lwt () = Lwt_unix.close fd in

    Hh_logger.info "Decompressing saved-state data";
    let%lwt (saved_state : serialized_t) =
      Profiling_js.with_timer_lwt profiling ~timer:"Decompress" ~f:(fun () ->
          try Lwt.return (Saved_state_compression.decompress_and_unmarshal compressed) with
          | exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to decompress saved state";
            raise (Invalid_saved_state Failed_to_decompress)
      )
    in

    Hh_logger.info "Loading saved-state data";
    let%lwt saved_state = load_saved_state ~options ~profiling saved_state in
    Hh_logger.info "Finished loading saved-state";

    Lwt.return saved_state
end

let save = Save.save

let load ~saved_state_filename ~options =
  let should_print_summary = Options.should_profile options in
  Profiling_js.with_profiling_lwt ~label:"LoadSavedState" ~should_print_summary (fun profiling ->
      Load.load ~saved_state_filename ~options ~profiling
  )
