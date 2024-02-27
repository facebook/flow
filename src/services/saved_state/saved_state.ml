(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type denormalized_file_data = {
  requires: string array;
  resolved_modules: Parsing_heaps.resolved_module array;
  phantom_dependencies: Modulename.t array;
  exports: Exports.t;
  hash: Xx.hash;
  imports: Imports.t;
}
[@@deriving show]

type normalized_file_data = denormalized_file_data [@@deriving show]

(* For each parsed file, this is what we will save *)
type parsed_file_data = {
  module_name: string option;
  normalized_file_data: normalized_file_data;
}
[@@deriving show]

(* We also need to store the info for unparsed files *)
type unparsed_file_data = {
  unparsed_module_name: string option;
  unparsed_hash: Xx.hash;
}

(** info for package.json files *)
type package_file_data = {
  package_module_name: string option;
  package_hash: Xx.hash;
  package_info: (Package_json.t, unit) result;
}

type saved_state_dependency_graph = File_key.t array * int array array * int array array

(* This is the complete saved state data representation *)
type saved_state_data = {
  (* The version header should guarantee that a saved state is used by the same version of Flow.
   * However, config might have changed in a way that invalidates the saved state. In the future,
   * we probably could allow some config options, whitespace, etc. But for now, let's
   * invalidate the saved state if the config has changed at all *)
  flowconfig_hash: Xx.hash;
  parsed_heaps: (File_key.t * parsed_file_data) list;
  unparsed_heaps: (File_key.t * unparsed_file_data) list;
  package_heaps: (File_key.t * package_file_data) list;  (** package.json info *)
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
  node_modules_containers: SSet.t SMap.t;
  dependency_graph: saved_state_dependency_graph;
}

let modulename_map_fn ~on_file ?on_string = function
  | Modulename.Filename fn -> Modulename.Filename (on_file fn)
  | Modulename.String str as module_name ->
    (match on_string with
    | None -> module_name
    | Some f -> Modulename.String (f str))

let resolved_module_map_fn ~on_file ?on_string = function
  | Ok mname -> Ok (modulename_map_fn ~on_file ?on_string mname)
  | Error mapped_name as err ->
    (match (mapped_name, on_string) with
    | (Some name, Some f) -> Error (Some (f name))
    | _ -> err)

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
    saved_state_filename:File_path.t ->
    genv:ServerEnv.genv ->
    env:ServerEnv.env ->
    profiling:Profiling_js.running ->
    unit Lwt.t
end = struct
  type t = {
    root: string;
    intern_tbl: (string, string) Hashtbl.t;
  }

  let make ~root = { root; intern_tbl = Hashtbl.create (1 lsl 20) }

  let intern t x =
    match Hashtbl.find_opt t.intern_tbl x with
    | Some interned -> interned
    | None ->
      Hashtbl.add t.intern_tbl x x;
      x

  (* We could also add a cache for this call, to improve sharing of the underlying strings
   * between file keys and the places that deal with raw paths. Unfortunately, an April 2020 test
   * of the saved state size of Facebook's largest JS codebase showed that while adding this
   * cache decreased the pre-compression size, it actually increased the post-compression size.
   * *)
  let normalize_path t path = intern t (Files.relative_path t.root path)

  let normalize_file_key t file_key = File_key.map (normalize_path t) file_key

  (* A Flow_error.t is a complicated data structure with Loc.t's hidden everywhere. *)
  let normalize_error t =
    Flow_error.map_loc_of_error (ALoc.update_source (Base.Option.map ~f:(normalize_file_key t)))

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

  let normalize_resolved_requires t resolved_modules phantom_dependencies =
    let on_file = normalize_file_key t in
    let on_string = intern t in
    let resolved_modules =
      Array.map (resolved_module_map_fn ~on_file ~on_string) resolved_modules
    in
    let phantom_dependencies =
      Array.map (modulename_map_fn ~on_file ~on_string) phantom_dependencies
    in
    (resolved_modules, phantom_dependencies)

  let rec normalize_exports t =
    let open Exports in
    let f = function
      | DefaultType name_opt -> DefaultType (Base.Option.map ~f:(intern t) name_opt)
      | Default name_opt -> Default (Base.Option.map ~f:(intern t) name_opt)
      | Named str -> Named (intern t str)
      | NamedType str -> NamedType (intern t str)
      | Module (str, exports) -> Module (intern t str, normalize_exports t exports)
    in
    (fun exports -> Base.List.map ~f exports)

  let normalize_imports t imports =
    let open Imports in
    let f { export; source; kind } =
      let export = intern t export in
      let source =
        match source with
        | Unresolved_source str -> Unresolved_source (intern t str)
        | Global -> Global
      in
      { export; source; kind }
    in
    Base.List.map ~f imports

  let normalize_file_data
      t { requires; resolved_modules; phantom_dependencies; exports; hash; imports } =
    let requires = Array.map (intern t) requires in
    let (resolved_modules, phantom_dependencies) =
      normalize_resolved_requires t resolved_modules phantom_dependencies
    in
    let exports = normalize_exports t exports in
    let imports = normalize_imports t imports in
    { requires; resolved_modules; phantom_dependencies; exports; hash; imports }

  let normalize_parsed_data t { module_name; normalized_file_data } =
    let module_name = Option.map (intern t) module_name in
    let normalized_file_data = normalize_file_data t normalized_file_data in
    { module_name; normalized_file_data }

  (* Collect all the data for a single parsed file *)
  let collect_normalized_data_for_parsed_file t ~reader fn parsed_heaps =
    if File_key.is_lib_file fn then
      parsed_heaps
    else
      let addr = Parsing_heaps.get_file_addr_unsafe fn in
      let parse = Parsing_heaps.Reader.get_typed_parse_unsafe ~reader fn addr in
      let imports = Parsing_heaps.read_imports parse in
      let requires = Parsing_heaps.read_requires parse in
      let resolved_requires = Parsing_heaps.Reader.get_resolved_requires_unsafe fn ~reader parse in
      let resolved_modules =
        Parsing_heaps.read_resolved_modules
          (Parsing_heaps.read_resolved_module Parsing_heaps.read_dependency)
          resolved_requires
      in
      let phantom_dependencies =
        Parsing_heaps.read_phantom_dependencies Parsing_heaps.read_dependency resolved_requires
      in
      let file_data =
        {
          module_name = Parsing_heaps.Reader.get_haste_name ~reader addr;
          normalized_file_data =
            {
              requires;
              resolved_modules;
              phantom_dependencies;
              exports = Parsing_heaps.read_exports parse;
              hash = Parsing_heaps.read_file_hash parse;
              imports;
            };
        }
      in
      let relative_fn = normalize_file_key t fn in
      let relative_file_data = normalize_parsed_data t file_data in
      (relative_fn, relative_file_data) :: parsed_heaps

  let collect_normalized_data_for_package_json_file t ~reader fn package_heaps =
    let addr = Parsing_heaps.get_file_addr_unsafe fn in
    let parse = Parsing_heaps.Reader.get_package_parse_unsafe ~reader fn addr in
    let package_module_name =
      Parsing_heaps.Reader.get_haste_name ~reader addr |> Option.map (intern t)
    in
    let relative_file_data =
      {
        package_module_name;
        package_hash = Parsing_heaps.read_file_hash parse;
        package_info = Parsing_heaps.read_package_info parse;
      }
    in
    let relative_fn = normalize_file_key t fn in
    (relative_fn, relative_file_data) :: package_heaps

  (* Collect all the data for a single unparsed file *)
  let collect_normalized_data_for_unparsed_file t ~reader fn unparsed_heaps =
    if File_key.is_lib_file fn then
      unparsed_heaps
    else
      let addr = Parsing_heaps.get_file_addr_unsafe fn in
      let parse = Parsing_heaps.Reader.get_parse_unsafe ~reader fn addr in
      let unparsed_module_name =
        Parsing_heaps.Reader.get_haste_name ~reader addr |> Option.map (intern t)
      in
      let relative_file_data =
        { unparsed_module_name; unparsed_hash = Parsing_heaps.read_file_hash parse }
      in
      let relative_fn = normalize_file_key t fn in
      (relative_fn, relative_file_data) :: unparsed_heaps

  (* The builtin flowlibs are excluded from the saved state. The server which loads the saved state
   * will extract and typecheck its own builtin flowlibs *)
  let is_not_in_flowlib ~options =
    let file_options = Options.file_options options in
    let is_in_flowlib = Files.is_in_flowlib file_options in
    (fun f -> not (is_in_flowlib f))

  let normalize_error_set t = Flow_error.ErrorSet.map (normalize_error t)

  (* Collect all the data for all the files *)
  let collect_data ~genv ~env ~profiling =
    let options = genv.ServerEnv.options in
    let reader = State_reader.create () in
    let root = Options.root options |> File_path.to_string in
    let t = make ~root in
    let parsed_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectParsed" ~f:(fun () ->
          FilenameSet.fold
            (collect_normalized_data_for_parsed_file t ~reader)
            env.ServerEnv.files
            []
      )
    in
    let unparsed_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectUnparsed" ~f:(fun () ->
          FilenameSet.fold
            (collect_normalized_data_for_unparsed_file t ~reader)
            env.ServerEnv.unparsed
            []
      )
    in
    let package_heaps =
      Profiling_js.with_timer profiling ~timer:"CollectPackageJson" ~f:(fun () ->
          FilenameSet.fold
            (collect_normalized_data_for_package_json_file t ~reader)
            env.ServerEnv.package_json_files
            []
      )
    in
    let ordered_non_flowlib_libs =
      env.ServerEnv.ordered_libs
      |> List.filter (is_not_in_flowlib ~options)
      |> Base.List.map ~f:(normalize_path t)
    in
    let local_errors =
      FilenameMap.fold
        (fun fn error_set acc ->
          let normalized_fn = normalize_file_key t fn in
          let normalized_error_set = normalize_error_set t error_set in
          FilenameMap.add normalized_fn normalized_error_set acc)
        env.ServerEnv.errors.ServerEnv.local_errors
        FilenameMap.empty
    in
    let node_modules_containers =
      SMap.fold
        (fun key value acc -> SMap.add (normalize_path t key) value acc)
        !Files.node_modules_containers
        SMap.empty
    in
    let dependency_graph =
      let dependency_info = env.ServerEnv.dependency_info in
      let dependency_info =
        Dependency_info.update
          dependency_info
          FilenameMap.empty
          (SSet.fold
             (fun n -> FilenameSet.add (File_key.LibFile n))
             env.ServerEnv.libs
             FilenameSet.empty
          )
      in
      let impl_map =
        dependency_info |> Dependency_info.implementation_dependency_graph |> FilenameGraph.to_map
      in
      let sig_map =
        dependency_info |> Dependency_info.sig_dependency_graph |> FilenameGraph.to_map
      in
      let files = Array.of_list (FilenameMap.keys impl_map) in
      let file_offset file_key =
        Base.Array.binary_search files ~compare:File_key.compare `First_equal_to file_key
        |> Option.get
      in
      let file_offsets (_, file_set) =
        FilenameSet.elements file_set |> Base.Array.of_list_map ~f:file_offset
      in
      let file_impls = FilenameMap.bindings impl_map |> Base.Array.of_list_map ~f:file_offsets in
      let file_sigs = FilenameMap.bindings sig_map |> Base.Array.of_list_map ~f:file_offsets in
      let files = Array.map (normalize_file_key t) files in
      (files, file_impls, file_sigs)
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
        node_modules_containers;
        dependency_graph;
      }

  let save ~saved_state_filename ~genv ~env ~profiling =
    Hh_logger.info "Collecting data for saved state";

    let%lwt data = collect_data ~genv ~env ~profiling in
    let filename = File_path.to_string saved_state_filename in
    Files.mkdirp (Filename.dirname filename) 0o777;
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
            Lwt.return compressed
          )
      )
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

        Lwt.return_unit
    )
end

type invalid_reason =
  | Bad_header
  | Build_mismatch of {
      expected: string;
      actual: string;
    }
  | Changed_files
  | Failed_to_marshal of Exception.t
  | Failed_to_decompress of Exception.t
  | File_does_not_exist
  | Flowconfig_mismatch

let invalid_reason_to_string = function
  | Bad_header -> "Invalid saved state header"
  | Build_mismatch _ -> "Build ID of saved state does not match this binary"
  | Changed_files -> "A file change invalidated the saved state"
  | Failed_to_marshal _ -> "Failed to unmarshal data from saved state"
  | Failed_to_decompress _ -> "Failed to decompress saved state data"
  | File_does_not_exist -> "Saved state file does not exist"
  | Flowconfig_mismatch -> ".flowconfig has changed since saved state was generated"

let backtrace_of_invalid_reason = function
  | Failed_to_decompress exn
  | Failed_to_marshal exn ->
    Some (Exception.to_string exn)
  | Build_mismatch { expected; actual } ->
    Some (Printf.sprintf "Expected %S, got %s" expected actual)
  | Bad_header
  | Changed_files
  | File_does_not_exist
  | Flowconfig_mismatch ->
    None

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
    saved_state_filename:File_path.t ->
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

  let denormalize_dependency_graph ~denormalizer (files, impls, sigs) =
    let files = Array.map (FileDenormalizer.denormalize_file_key denormalizer) files in
    (files, impls, sigs)

  let denormalize_error ~denormalizer =
    Flow_error.map_loc_of_error
      (ALoc.update_source (Base.Option.map ~f:(FileDenormalizer.denormalize_file_key denormalizer)))

  let verify_version =
    (* Flow_build_id should always be 16 bytes *)
    let rec read_version fd buf offset len =
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
      if len > 0 then
        read_version fd buf offset len
      else
        Lwt.return (Bytes.to_string buf)
    in
    let assert_version actual =
      let expected = saved_state_version () in
      if actual <> expected then (
        Hh_logger.error
          "Saved-state file failed version check. Expected version %S but got %S"
          expected
          actual;
        raise (Invalid_saved_state (Build_mismatch { expected; actual }))
      ) else
        ()
    in
    fun options fd ->
      let%lwt version =
        read_version fd (Bytes.create saved_state_version_length) 0 saved_state_version_length
      in
      if Options.saved_state_skip_version_check options then
        (* This is really unsafe! Saved state is marshal'd OCaml data and it's
           easy to introduce serialization differences that would lead to
           segfaults. This is only for debugging.

           We still have to read the version because it consumes from the fd. *)
        Lwt.return_unit
      else
        Lwt.return (assert_version version)

  let denormalize_resolved_requires ~root resolved_modules phantom_dependencies =
    let resolved_modules =
      Array.map
        (resolved_module_map_fn ~on_file:(denormalize_file_key_nocache ~root))
        resolved_modules
    in
    let phantom_dependencies =
      Array.map
        (modulename_map_fn ~on_file:(denormalize_file_key_nocache ~root))
        phantom_dependencies
    in
    (* Sort after denormalizing because the denormalized file keys could be in a
     * different sort order, specifically for paths outside of the root, i.e.,
     * paths starting with `../` *)
    Array.sort Modulename.compare phantom_dependencies;
    (resolved_modules, phantom_dependencies)

  (** Turns all the relative paths in a file's data back into absolute paths. *)
  let denormalize_file_data
      ~root { requires; resolved_modules; phantom_dependencies; exports; hash; imports } =
    let (resolved_modules, phantom_dependencies) =
      denormalize_resolved_requires ~root resolved_modules phantom_dependencies
    in
    { requires; resolved_modules; phantom_dependencies; exports; hash; imports }

  let progress_fn real_total ~total:_ ~start ~length:_ =
    MonitorRPC.status_update
      ~event:ServerStatus.(Load_saved_state_progress { total = Some real_total; finished = start })

  (* Denormalize the data for all the parsed files. This is kind of slow :( *)
  let denormalize_parsed_heaps ~denormalizer parsed_heaps =
    Base.List.map
      ~f:(fun (relative_fn, parsed_file_data) ->
        let fn = FileDenormalizer.denormalize_file_key denormalizer relative_fn in
        (fn, parsed_file_data))
      parsed_heaps

  (* Denormalize the data for all the unparsed files *)
  let denormalize_unparsed_heaps
      ~workers ~blocking_worker_communication ~root ~progress_fn unparsed_heaps =
    let next = MultiWorkerLwt.next ~progress_fn ~max_size:4000 workers unparsed_heaps in
    let job acc (relative_fn, unparsed_file_data) =
      let fn = denormalize_file_key_nocache ~root relative_fn in
      (fn, unparsed_file_data) :: acc
    in
    MultiWorkerLwt.fold
      workers
      ~blocking:blocking_worker_communication
      ~job
      ~neutral:[]
      ~merge:List.rev_append
      ~next

  let denormalize_package_heaps ~denormalizer package_heaps =
    Base.List.map
      ~f:(fun (relative_fn, package) ->
        let fn = FileDenormalizer.denormalize_file_key denormalizer relative_fn in
        (fn, package))
      package_heaps

  let denormalize_error_set ~denormalizer = Flow_error.ErrorSet.map (denormalize_error ~denormalizer)

  (* Denormalize all the data *)
  let denormalize_data ~workers ~options ~data =
    let root = Options.root options |> File_path.to_string in
    let denormalizer = FileDenormalizer.make ~root in
    let {
      flowconfig_hash;
      parsed_heaps;
      unparsed_heaps;
      package_heaps;
      ordered_non_flowlib_libs;
      local_errors;
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

    Hh_logger.info "Denormalizing the data for the package.json files";
    let package_heaps = denormalize_package_heaps ~denormalizer package_heaps in
    Hh_logger.info "Denormalizing the data for the parsed files";
    let parsed_heaps = denormalize_parsed_heaps ~denormalizer parsed_heaps in
    Hh_logger.info "Denormalizing the data for the unparsed files";
    let%lwt unparsed_heaps =
      let progress_fn = progress_fn (List.length unparsed_heaps) in
      denormalize_unparsed_heaps
        ~blocking_worker_communication:(Options.blocking_worker_communication options)
        ~workers
        ~root
        ~progress_fn
        unparsed_heaps
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
        node_modules_containers;
        dependency_graph;
      }

  let load ~workers ~saved_state_filename ~options ~profiling =
    let filename = File_path.to_string saved_state_filename in
    Hh_logger.info "Reading saved-state file at %S" filename;

    MonitorRPC.status_update ~event:ServerStatus.Read_saved_state;

    let%lwt fd =
      try%lwt Lwt_unix.openfile filename [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o666 with
      | Unix.Unix_error (Unix.ENOENT, _, _) as exn ->
        let exn = Exception.wrap exn in
        Hh_logger.error "Failed to open %S\n%s" filename (Exception.to_string exn);
        raise (Invalid_saved_state File_does_not_exist)
    in
    let%lwt () = verify_version options fd in
    let%lwt (compressed_data : Saved_state_compression.compressed) =
      Profiling_js.with_timer_lwt profiling ~timer:"Read" ~f:(fun () ->
          try%lwt Marshal_tools_lwt.from_fd_with_preamble fd with
          | exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to parse saved state data";
            raise (Invalid_saved_state (Failed_to_marshal exn))
      )
    in
    let%lwt () = Lwt_unix.close fd in
    Hh_logger.info "Decompressing saved-state data";

    let%lwt (data : saved_state_data) =
      Profiling_js.with_timer_lwt profiling ~timer:"Decompress" ~f:(fun () ->
          try Lwt.return (Saved_state_compression.decompress_and_unmarshal compressed_data) with
          | exn ->
            let exn = Exception.wrap exn in
            Hh_logger.error ~exn "Failed to decompress saved state";
            raise (Invalid_saved_state (Failed_to_decompress exn))
      )
    in
    Hh_logger.info "Denormalizing saved-state data";

    let%lwt data =
      Profiling_js.with_timer_lwt profiling ~timer:"Denormalize" ~f:(fun () ->
          denormalize_data ~workers ~options ~data
      )
    in
    Hh_logger.info "Finished loading saved-state";

    Lwt.return data
end

let save = Save.save

let load ~workers ~saved_state_filename ~options =
  let should_print_summary = Options.should_profile options in
  Profiling_js.with_profiling_lwt ~label:"LoadSavedState" ~should_print_summary (fun profiling ->
      Load.load ~workers ~saved_state_filename ~options ~profiling
  )

let denormalize_file_data = Load.denormalize_file_data

let restore_dependency_info (files, file_impls, file_sigs) =
  let dep_set_of_offsets dep_offsets =
    let dep_files = Array.map (fun i -> files.(i)) dep_offsets in
    (* Sort after denormalizing because the denormalized file keys could be in a
     * different sort order, specifically for paths outside of the root, i.e.,
     * paths starting with `../` *)
    Array.sort File_key.compare dep_files;
    FilenameSet.of_sorted_array_unchecked dep_files
  in
  let files =
    Array.mapi
      (fun i file ->
        let impl_deps = dep_set_of_offsets file_impls.(i) in
        let sig_deps = dep_set_of_offsets file_sigs.(i) in
        (file, (sig_deps, impl_deps)))
      files
  in
  (* Sort after denormalizing because the denormalized file keys could be in a
   * different sort order, specifically for paths outside of the root, i.e.,
   * paths starting with `../` *)
  Array.sort (fun (a, _) (b, _) -> File_key.compare a b) files;
  FilenameMap.of_sorted_array_unchecked files |> Dependency_info.of_map
