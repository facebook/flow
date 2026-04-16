(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type denormalized_file_data = {
  requires: Flow_import_specifier.t array;
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
  haste_module_info: Haste_module_info.t option;
  normalized_file_data: normalized_file_data;
}
[@@deriving show]

(* We also need to store the info for unparsed files *)
type unparsed_file_data = {
  unparsed_haste_module_info: Haste_module_info.t option;
  unparsed_hash: Xx.hash;
}

(** info for package.json files *)
type package_file_data = {
  package_haste_module_info: Haste_module_info.t option;
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
  non_flowlib_libs: SSet.t;
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

(* Direct serialization saved state data: the shared memory heap is dumped
 * directly to disk via SharedMem.save_heap. This type holds only lightweight
 * env-level metadata (file sets, dependency graph, etc.) — the per-file data
 * lives in the heap dump itself. On load, SharedMem.load_heap bulk-loads the
 * heap, so no per-file restoration is needed. *)
type saved_state_env_data = {
  flowconfig_hash: Xx.hash;
  parsed_files: FilenameSet.t;
  unparsed_files: FilenameSet.t;
  package_json_files: FilenameSet.t;
  non_flowlib_libs: SSet.t;
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  node_modules_containers: SSet.t SMap.t;
  dependency_info: Dependency_info.t;
  duplicate_providers: (File_key.t * File_key.t Nel.t) SMap.t;
  export_index: Export_index.t option;
}

type loaded_saved_state =
  | Legacy_saved_state of saved_state_data
  | Direct_saved_state of saved_state_env_data

let modulename_map_fn ~on_file ?on_string = function
  | Modulename.Filename fn -> Modulename.Filename (on_file fn)
  | Modulename.Haste haste_module_info as module_name ->
    (match on_string with
    | None -> module_name
    | Some f ->
      Modulename.Haste
        (Haste_module_info.mk
           ~module_name:(f (Haste_module_info.module_name haste_module_info))
           ~namespace_bitset:(Haste_module_info.namespace_bitset haste_module_info)
        ))

let import_specifier_map_fn ~on_string = function
  | Flow_import_specifier.Userland name ->
    Flow_import_specifier.Userland (Flow_import_specifier.map_userland ~f:on_string name)
  | Flow_import_specifier.HasteImportWithSpecifiedNamespace
      { namespace; name; allow_implicit_platform_specific_import } ->
    Flow_import_specifier.HasteImportWithSpecifiedNamespace
      { namespace; name = on_string name; allow_implicit_platform_specific_import }

let resolved_module_map_fn ~on_file ?on_string = function
  | Ok mname -> Ok (modulename_map_fn ~on_file ?on_string mname)
  | Error mapped_name as err ->
    (match (mapped_name, on_string) with
    | (Some name, Some on_string) -> Error (Some (import_specifier_map_fn ~on_string name))
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

(* We write the Flow version at the beginning of each saved state file. *)
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

let normalize_path root path = Files.relative_path root path

let collect_flowconfig_hash ~options =
  FlowConfig.get_hash
  @@ Server_files_js.config_file (Options.flowconfig_name options)
  @@ Options.root options

let collect_non_flowlib_libs ~env ~options root =
  let is_in_flowlib =
    let file_options = Options.file_options options in
    Files.is_in_flowlib file_options
  in
  env.ServerEnv.all_unordered_libs
  |> SSet.filter (fun lib -> not (is_in_flowlib lib))
  |> SSet.map (normalize_path root)

let collect_node_modules_containers root =
  SMap.fold
    (fun key value acc -> SMap.add (normalize_path root key) value acc)
    !Files.node_modules_containers
    SMap.empty

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
  (* Legacy save: this is the exact original Save module code. It must remain
   * unchanged to guarantee behavioral identity with the pre-gating code path. *)
  module Legacy = struct
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

    let intern_userland_import_specifier t = Flow_import_specifier.map_userland ~f:(intern t)

    let intern_haste_module_info t haste_module_info =
      let _module_name : string = intern t (Haste_module_info.module_name haste_module_info) in
      haste_module_info

    (* We could also add a cache for this call, to improve sharing of the underlying strings
     * between file keys and the places that deal with raw paths. Unfortunately, an April 2020 test
     * of the saved state size of Facebook's largest JS codebase showed that while adding this
     * cache decreased the pre-compression size, it actually increased the post-compression size.
     * *)
    let normalize_path t path = intern t (Files.relative_path t.root path)

    (* With relative paths in File_key.t, no path transformation is needed.
       However, we still intern the suffix strings to ensure physical sharing:
       OCaml's Marshal uses pointer identity to detect shared values, and without
       interning, identical path strings (e.g., popular resolved modules like React
       referenced from hundreds of thousands of files) become separate heap objects,
       inflating the marshaled size past the ~2GB LZ4 limit. *)
    let normalize_file_key t file_key = File_key.map (intern t) file_key

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
        | Module (s, exports) ->
          Module (intern_userland_import_specifier t s, normalize_exports t exports)
        | ReExportModule s -> ReExportModule (intern_userland_import_specifier t s)
        | ReExportModuleTypes s -> ReExportModuleTypes (intern_userland_import_specifier t s)
      in
      (fun exports -> Base.List.map ~f exports)

    let normalize_imports t imports =
      let open Imports in
      let f { export; source; kind } =
        let export = intern t export in
        let source =
          match source with
          | Unresolved_source s -> Unresolved_source (intern_userland_import_specifier t s)
          | Global -> Global
        in
        { export; source; kind }
      in
      Base.List.map ~f imports

    let normalize_file_data
        t { requires; resolved_modules; phantom_dependencies; exports; hash; imports } =
      let requires = Array.map (import_specifier_map_fn ~on_string:(intern t)) requires in
      let (resolved_modules, phantom_dependencies) =
        normalize_resolved_requires t resolved_modules phantom_dependencies
      in
      let exports = normalize_exports t exports in
      let imports = normalize_imports t imports in
      { requires; resolved_modules; phantom_dependencies; exports; hash; imports }

    let normalize_parsed_data t { haste_module_info; normalized_file_data } =
      let haste_module_info = Option.map (intern_haste_module_info t) haste_module_info in
      let normalized_file_data = normalize_file_data t normalized_file_data in
      { haste_module_info; normalized_file_data }

    (* Collect all the data for a single parsed file *)
    let collect_normalized_data_for_parsed_file t ~reader fn parsed_heaps =
      if File_key.is_lib_file fn then
        parsed_heaps
      else
        let addr = Parsing_heaps.get_file_addr_unsafe fn in
        let parse = Parsing_heaps.Reader.get_typed_parse_unsafe ~reader fn addr in
        let imports = Parsing_heaps.read_imports parse in
        let requires = Parsing_heaps.read_requires parse in
        let resolved_requires =
          Parsing_heaps.Reader.get_resolved_requires_unsafe fn ~reader parse
        in
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
            haste_module_info = Parsing_heaps.Reader.get_haste_module_info ~reader addr;
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
      let package_haste_module_info =
        Parsing_heaps.Reader.get_haste_module_info ~reader addr
        |> Option.map (intern_haste_module_info t)
      in
      let relative_file_data =
        {
          package_haste_module_info;
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
        let unparsed_haste_module_info =
          Parsing_heaps.Reader.get_haste_module_info ~reader addr
          |> Option.map (intern_haste_module_info t)
        in
        let relative_file_data =
          { unparsed_haste_module_info; unparsed_hash = Parsing_heaps.read_file_hash parse }
        in
        let relative_fn = normalize_file_key t fn in
        (relative_fn, relative_file_data) :: unparsed_heaps

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
      (* The builtin flowlibs are excluded from the saved state. The server which loads the saved
       * state will extract and typecheck its own builtin flowlibs *)
      let is_in_flowlib =
        let file_options = Options.file_options options in
        Files.is_in_flowlib file_options
      in
      let non_flowlib_libs =
        env.ServerEnv.all_unordered_libs
        |> SSet.filter (fun lib -> not (is_in_flowlib lib))
        |> SSet.map (normalize_path t)
      in
      let local_errors = env.ServerEnv.errors.ServerEnv.local_errors in
      let node_modules_containers =
        SMap.fold
          (fun key value acc -> SMap.add (normalize_path t key) value acc)
          !Files.node_modules_containers
          SMap.empty
      in
      let dependency_graph =
        let dependency_info = env.ServerEnv.dependency_info in
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
          non_flowlib_libs;
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
          Hh_logger.info
            "Finished writing %d bytes to saved-state file at %S"
            bytes_written
            filename;
          Lwt.return_unit
      )
  end

  (* Direct serialization save: nulls out lazy fields in heap objects, runs GC
   * + compact, then writes the raw heap bytes directly to disk followed by
   * compressed env metadata. The file format is:
   * [version header][raw heap dump][marshalled+compressed saved_state_env_data].
   * Much faster than legacy since it avoids per-file iteration and marshalling. *)
  module Direct = struct
    let collect_env_data ~genv ~env ~profiling =
      let options = genv.ServerEnv.options in
      let root = Options.root options |> File_path.to_string in
      let parsed_files =
        Profiling_js.with_timer profiling ~timer:"CollectParsed" ~f:(fun () ->
            FilenameSet.filter (fun fn -> not (File_key.is_lib_file fn)) env.ServerEnv.files
        )
      in
      let unparsed_files =
        Profiling_js.with_timer profiling ~timer:"CollectUnparsed" ~f:(fun () ->
            FilenameSet.filter (fun fn -> not (File_key.is_lib_file fn)) env.ServerEnv.unparsed
        )
      in
      let package_json_files = env.ServerEnv.package_json_files in
      let non_flowlib_libs = collect_non_flowlib_libs ~env ~options root in
      let local_errors = env.ServerEnv.errors.ServerEnv.local_errors in
      let duplicate_providers = env.ServerEnv.errors.ServerEnv.duplicate_providers in
      let node_modules_containers = collect_node_modules_containers root in
      let dependency_info = env.ServerEnv.dependency_info in
      let flowconfig_hash = collect_flowconfig_hash ~options in
      let export_index =
        if Options.saved_state_persist_export_index options then
          Option.map Export_search.get_index env.ServerEnv.exports
        else
          None
      in
      {
        flowconfig_hash;
        parsed_files;
        unparsed_files;
        package_json_files;
        non_flowlib_libs;
        local_errors;
        node_modules_containers;
        dependency_info;
        duplicate_providers;
        export_index;
      }

    let write_env ~profiling ~filename fd env_data =
      Hh_logger.info "Compressing env metadata with lz4";
      let%lwt saved_state_contents =
        Profiling_js.with_timer_lwt profiling ~timer:"Compress" ~f:(fun () ->
            Saved_state_compression.(
              let compressed = marshal_and_compress env_data in
              let orig_size = uncompressed_size compressed in
              let new_size = compressed_size compressed in
              Hh_logger.info
                "Compressed env data from %d bytes to %d bytes (%3.2f%%)"
                orig_size
                new_size
                (100. *. float_of_int new_size /. float_of_int orig_size);
              Lwt.return compressed
            )
        )
      in
      Profiling_js.with_timer_lwt profiling ~timer:"WriteEnv" ~f:(fun () ->
          Hh_logger.info "Writing env metadata to saved-state file at %S" filename;
          let%lwt _data_bytes_written =
            Marshal_tools_lwt.to_fd_with_preamble
              fd
              (saved_state_contents : Saved_state_compression.compressed)
          in
          Lwt.return_unit
      )

    let write_heap ~profiling fd =
      Hh_logger.info "Saving heap to saved-state file";
      Profiling_js.with_timer_lwt profiling ~timer:"SaveHeap" ~f:(fun () ->
          SharedMem.save_heap (Lwt_unix.unix_file_descr fd);
          Lwt.return_unit
      )

    let save ~saved_state_filename ~genv ~env ~profiling =
      Hh_logger.info "Collecting env data for saved state";
      let options = genv.ServerEnv.options in
      let env_data = collect_env_data ~genv ~env ~profiling in
      let filename = File_path.to_string saved_state_filename in
      Files.mkdirp (Filename.dirname filename) 0o777;
      let%lwt fd = Lwt_unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
      let%lwt _header_bytes_written = write_version fd in
      if Options.saved_state_parallel_decompress options then begin
        (* Write env metadata FIRST (before heap), so on load we can read it
         * and decompress in parallel with loading the heap dump. *)
        let%lwt () = write_env ~profiling ~filename fd env_data in
        write_heap ~profiling fd
      end else begin
        let%lwt () = write_heap ~profiling fd in
        write_env ~profiling ~filename fd env_data
      end;%lwt
      let%lwt () = Lwt_unix.close fd in
      Hh_logger.info "Finished writing saved-state file at %S" filename;
      Lwt.return_unit
  end

  let save ~saved_state_filename ~genv ~env ~profiling =
    let options = genv.ServerEnv.options in
    if Options.saved_state_direct_serialization options then
      Direct.save ~saved_state_filename ~genv ~env ~profiling
    else
      Legacy.save ~saved_state_filename ~genv ~env ~profiling
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
  | Failed_to_load_heap of string
  | File_does_not_exist
  | Flowconfig_mismatch

let invalid_reason_to_string = function
  | Bad_header -> "Invalid saved state header"
  | Build_mismatch _ -> "Build ID of saved state does not match this binary"
  | Changed_files -> "A file change invalidated the saved state"
  | Failed_to_marshal _ -> "Failed to unmarshal data from saved state"
  | Failed_to_decompress _ -> "Failed to decompress saved state data"
  | Failed_to_load_heap msg -> "Failed to load heap: " ^ msg
  | File_does_not_exist -> "Saved state file does not exist"
  | Flowconfig_mismatch -> ".flowconfig has changed since saved state was generated"

let backtrace_of_invalid_reason = function
  | Failed_to_decompress exn
  | Failed_to_marshal exn ->
    Some (Exception.to_string exn)
  | Build_mismatch { expected; actual } ->
    Some (Printf.sprintf "Expected %S, got %s" expected actual)
  | Failed_to_load_heap msg -> Some msg
  | Bad_header
  | Changed_files
  | File_does_not_exist
  | Flowconfig_mismatch ->
    None

exception Invalid_saved_state of invalid_reason

let verify_version =
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

let verify_flowconfig_hash ~options ~flowconfig_hash =
  let current_flowconfig_hash =
    let flowconfig_name = Options.flowconfig_name options in
    FlowConfig.get_hash @@ Server_files_js.config_file flowconfig_name @@ Options.root options
  in
  if
    (not (Options.saved_state_skip_version_check options))
    && flowconfig_hash <> current_flowconfig_hash
  then (
    Hh_logger.error
      "Invalid saved state: .flowconfig has changed since this saved state was generated.";
    raise (Invalid_saved_state Flowconfig_mismatch)
  )

let denormalize_paths ~options ~non_flowlib_libs ~node_modules_containers =
  let root = Options.root options |> File_path.to_string in
  let prepend_root path = Files.absolute_path root path in
  let non_flowlib_libs = SSet.map prepend_root non_flowlib_libs in
  let node_modules_containers =
    SMap.fold
      (fun key value acc -> SMap.add (prepend_root key) value acc)
      node_modules_containers
      SMap.empty
  in
  (non_flowlib_libs, node_modules_containers)

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
    loaded_saved_state Lwt.t

  val denormalize_file_data : options:Options.t -> normalized_file_data -> denormalized_file_data
end = struct
  (* With relative paths in File_key.t, denormalization is identity. The data
     was already sorted on the save side, and no path transformation changes
     the sort order. *)
  let denormalize_file_data ~options:_ data = data

  (* Legacy load: this is the exact original Load module code. It must remain
   * unchanged to guarantee behavioral identity with the pre-gating code path. *)
  module Legacy = struct
    (* Validate saved state and prepare data for use.
       With relative paths in File_key.t, the saved state's relative paths are used
       directly — no per-file denormalization needed. Only raw string paths
       (non_flowlib_libs, node_modules_containers) need root prepending. *)
    let denormalize_data ~workers:_ ~options ~data =
      (* Signal progress so the status state machine transitions to Loading_saved_state,
         which is required before the Restoring_heaps_start event can fire. *)
      MonitorRPC.status_update
        ~event:ServerStatus.(Load_saved_state_progress { total = None; finished = 0 });
      let {
        flowconfig_hash;
        parsed_heaps;
        unparsed_heaps;
        package_heaps;
        non_flowlib_libs;
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
      if
        (not (Options.saved_state_skip_version_check options))
        && flowconfig_hash <> current_flowconfig_hash
      then (
        Hh_logger.error
          "Invalid saved state: .flowconfig has changed since this saved state was generated.";
        raise (Invalid_saved_state Flowconfig_mismatch)
      );

      (* Raw string paths still need the root prepended *)
      let root = Options.root options |> File_path.to_string in
      let prepend_root path = Files.absolute_path root path in
      let non_flowlib_libs = SSet.map prepend_root non_flowlib_libs in
      let node_modules_containers =
        SMap.fold
          (fun key value acc -> SMap.add (prepend_root key) value acc)
          node_modules_containers
          SMap.empty
      in
      Lwt.return
        {
          flowconfig_hash;
          parsed_heaps;
          unparsed_heaps;
          package_heaps;
          non_flowlib_libs;
          local_errors;
          node_modules_containers;
          dependency_graph;
        }

    let load ~workers ~options ~profiling fd =
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
      Lwt.return (Legacy_saved_state data)
  end

  module Direct = struct
    let denormalize_env_data ~options ~data =
      MonitorRPC.status_update
        ~event:ServerStatus.(Load_saved_state_progress { total = None; finished = 0 });
      let {
        flowconfig_hash;
        parsed_files;
        unparsed_files;
        package_json_files;
        non_flowlib_libs;
        local_errors;
        node_modules_containers;
        dependency_info;
        duplicate_providers;
        export_index;
      } =
        data
      in
      verify_flowconfig_hash ~options ~flowconfig_hash;
      let (non_flowlib_libs, node_modules_containers) =
        denormalize_paths ~options ~non_flowlib_libs ~node_modules_containers
      in
      {
        flowconfig_hash;
        parsed_files;
        unparsed_files;
        package_json_files;
        non_flowlib_libs;
        local_errors;
        node_modules_containers;
        dependency_info;
        duplicate_providers;
        export_index;
      }

    let load_heap ~profiling fd =
      Profiling_js.with_timer_lwt profiling ~timer:"LoadHeap" ~f:(fun () ->
          (try SharedMem.load_heap (Lwt_unix.unix_file_descr fd) with
          | Failure msg ->
            Hh_logger.error "Failed to load heap: %s" msg;
            raise (Invalid_saved_state (Failed_to_load_heap msg)));
          Lwt.return_unit
      )

    let read_env fd =
      try%lwt Marshal_tools_lwt.from_fd_with_preamble fd with
      | exn ->
        let exn = Exception.wrap exn in
        Hh_logger.error ~exn "Failed to parse saved state env data";
        raise (Invalid_saved_state (Failed_to_marshal exn))

    let decompress_env ~options ~release_lock compressed_data =
      let decompress =
        if release_lock then
          Saved_state_compression.decompress_and_unmarshal_releasing_lock
        else
          Saved_state_compression.decompress_and_unmarshal
      in
      let data =
        try decompress compressed_data with
        | exn ->
          let exn = Exception.wrap exn in
          Hh_logger.error ~exn "Failed to decompress saved state env data";
          raise (Invalid_saved_state (Failed_to_decompress exn))
      in
      denormalize_env_data ~options ~data

    (* Sequential load: file format is [version][heap][env].
     * Load heap first, then read and decompress env. *)
    let load_sequential ~options ~profiling fd =
      Hh_logger.info "Loading heap from saved-state file";
      let%lwt () = load_heap ~profiling fd in
      Hh_logger.info "Reading env metadata from saved-state file";
      let%lwt (compressed_data : Saved_state_compression.compressed) =
        Profiling_js.with_timer_lwt profiling ~timer:"Read" ~f:(fun () -> read_env fd)
      in
      let%lwt () = Lwt_unix.close fd in
      Hh_logger.info "Decompressing env metadata";
      let%lwt data =
        Profiling_js.with_timer_lwt profiling ~timer:"Decompress" ~f:(fun () ->
            Lwt.return (decompress_env ~options ~release_lock:false compressed_data)
        )
      in
      Hh_logger.info "Finished loading saved-state";
      Lwt.return (Direct_saved_state data)

    (* Parallel load: file format is [version][env][heap].
     * Read env first, decompress in background thread while loading heap
     * on the main thread. Both C stubs release the OCaml runtime lock,
     * enabling true parallel execution on separate OS threads. *)
    let load_parallel ~options ~profiling fd =
      Hh_logger.info "Reading env metadata from saved-state file";
      let%lwt (compressed_data : Saved_state_compression.compressed) =
        Profiling_js.with_timer_lwt profiling ~timer:"Read" ~f:(fun () -> read_env fd)
      in
      Hh_logger.info "Decompressing env metadata + loading heap in parallel";
      let decompress_result = ref None in
      let decompress_exn = ref None in
      let decompress_done = Mutex.create () in
      let decompress_cond = Condition.create () in
      let _decompress_thread =
        Thread.create
          (fun () ->
            (try
               decompress_result := Some (decompress_env ~options ~release_lock:true compressed_data)
             with
            | exn -> decompress_exn := Some (Exception.wrap exn));
            Mutex.lock decompress_done;
            Condition.signal decompress_cond;
            Mutex.unlock decompress_done)
          ()
      in
      let%lwt () =
        Profiling_js.with_timer_lwt profiling ~timer:"LoadHeapAndDecompress" ~f:(fun () ->
            (try SharedMem.load_heap (Lwt_unix.unix_file_descr fd) with
            | Failure msg ->
              Hh_logger.error "Failed to load heap: %s" msg;
              raise (Invalid_saved_state (Failed_to_load_heap msg)));
            Mutex.lock decompress_done;
            while !decompress_result = None && !decompress_exn = None do
              Condition.wait decompress_cond decompress_done
            done;
            Mutex.unlock decompress_done;
            Lwt.return_unit
        )
      in
      let%lwt () = Lwt_unix.close fd in
      let data =
        match (!decompress_result, !decompress_exn) with
        | (Some data, _) -> data
        | (_, Some exn) -> Exception.reraise exn
        | (None, None) -> failwith "decompress thread finished without result"
      in
      Hh_logger.info "Finished loading saved-state";
      Lwt.return (Direct_saved_state data)

    let load ~options ~profiling fd =
      if Options.saved_state_parallel_decompress options then
        load_parallel ~options ~profiling fd
      else
        load_sequential ~options ~profiling fd
  end

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
    if Options.saved_state_direct_serialization options then
      Direct.load ~options ~profiling fd
    else
      Legacy.load ~workers ~options ~profiling fd
end

let save = Save.save

let load ~workers ~saved_state_filename ~options =
  let should_print_summary = Options.should_profile options in
  Profiling_js.with_profiling_lwt ~label:"LoadSavedState" ~should_print_summary (fun profiling ->
      Load.load ~workers ~saved_state_filename ~options ~profiling
  )

let non_flowlib_libs = function
  | Legacy_saved_state data -> data.non_flowlib_libs
  | Direct_saved_state data -> data.non_flowlib_libs

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
