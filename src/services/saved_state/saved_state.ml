(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Utils_js

(* For each parsed file, this is what we will save *)
type parsed_file_data = {
  package: Package_json.t option; (* Only package.json files have this *)
  info: Module_heaps.info;
  file_sig: File_sig.t;
  resolved_requires: Module_heaps.resolved_requires;
  hash: Xx.hash;
}

(* We also need to store the info for unparsed files *)
type unparsed_file_data = {
  unparsed_info: Module_heaps.info;
  unparsed_hash: Xx.hash;
}

(* This is the complete saved state data representation *)
type saved_state_data = {
  (* The version header should guarantee that a saved state is used by the same version of Flow.
   * However, config might have changed in a way that invalidates the saved state. In the future,
   * we probably could whitelist some config options, whitespace, etc. But for now, let's
   * invalidate the saved state if the config has changed at all *)
  flowconfig_hash: Xx.hash;

  parsed_heaps: parsed_file_data FilenameMap.t;
  unparsed_heaps: unparsed_file_data FilenameMap.t;
  ordered_non_flowlib_libs: string list;

  (* Why store local errors and not merge_errors/suppressions/etc? Well, I have a few reasons:
   *
   * 1. Much smaller data structure. The whole env.errors data structure can be hundreds of MBs
   *    when marshal'd, even when there are 0 errors reported to the user)
   * 2. Saved state is designed to help skip parsing. One of the outputs of parsing are local errors
   * 3. Local errors should be the same after a lazy init and after a full init. This isn't true
   *    for the other members of env.errors which are filled in during typechecking
   *)
  local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;

  node_modules_containers: SSet.t;

  (* TODO - Figure out what to do aboute module.resolver *)
}

let modulename_map_fn ~f = function
| Modulename.Filename fn -> Modulename.Filename (f fn)
| Modulename.String _ as module_name -> module_name

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
module Save: sig
  val save:
    saved_state_filename:Path.t ->
    genv:ServerEnv.genv ->
    env:ServerEnv.env ->
    unit Lwt.t
end = struct
  let normalize_file_key ~root = File_key.map (Files.relative_path root)

  (* A File_sig.t is a complicated data structure with Loc.t's hidden everywhere. The easiest way to
   * make sure we get them all is with a mapper *)
  class file_sig_normalizer (root) = object
    inherit File_sig.mapper

    method! loc (loc: Loc.t) =
      { loc with
        Loc.source = Option.map ~f:(normalize_file_key ~root) loc.Loc.source;
      }
  end

  (* A Error.error is a complicated data structure with Loc.t's hidden everywhere. The easiest way
   * to make sure we get them all is with a mapper *)
  class error_normalizer (root) = object
    inherit Errors.mapper

    method! loc (loc: Loc.t) =
      { loc with
        Loc.source = Option.map ~f:(normalize_file_key ~root) loc.Loc.source;
      }
  end

  (* We write the Flow version at the beginning of each saved state file. It's an easy way to assert
   * upon reading the file that the writer and reader are the same version of Flow *)
  let write_version =
    let version_length = String.length Flow_version.version in
    let rec write_version fd offset len =
      if len > 0
      then
        let%lwt bytes_written = Lwt_unix.write_string fd Flow_version.version offset len in
        let offset = offset + bytes_written in
        let len = len - bytes_written in
        write_version fd offset len
      else
        Lwt.return version_length
    in
    fun fd -> write_version fd 0 version_length

  let normalize_info ~root info =
    let module_name =
      modulename_map_fn ~f:(normalize_file_key ~root) info.Module_heaps.module_name
    in
    { info with Module_heaps.module_name }

  let normalize_parsed_data ~root parsed_file_data =
    (* info *)
    let info = normalize_info ~root parsed_file_data.info in

    (* file_sig *)
    let file_sig = (new file_sig_normalizer root)#file_sig parsed_file_data.file_sig in

    (* resolved_requires *)
    let { Module_heaps.resolved_modules; phantom_dependents } =
      parsed_file_data.resolved_requires
    in
    let phantom_dependents = SSet.map (Files.relative_path root) phantom_dependents in
    let resolved_modules = SMap.map
      (modulename_map_fn ~f:(normalize_file_key ~root)) resolved_modules in
    let resolved_requires = { Module_heaps.resolved_modules; phantom_dependents } in

    {
      package = parsed_file_data.package;
      info;
      file_sig;
      resolved_requires;
      hash = parsed_file_data.hash;
    }

  (* Collect all the data for a single parsed file *)
  let collect_normalized_data_for_parsed_file ~root parsed_heaps fn =
    let package =
      match fn with
      | File_key.JsonFile str when Filename.basename str = "package.json" ->
          Some (Module_heaps.For_saved_state.get_package_json_unsafe str)
      | _ -> None
    in

    let file_data = {
      package;
      info = Module_heaps.get_info_unsafe ~audit:Expensive.ok fn;
      file_sig = Parsing_heaps.get_file_sig_unsafe fn;
      resolved_requires = Module_heaps.get_resolved_requires_unsafe ~audit:Expensive.ok fn;
      hash = Parsing_heaps.get_file_hash_unsafe fn;
    } in

    let relative_fn = normalize_file_key ~root fn in

    let relative_file_data = normalize_parsed_data ~root file_data in

    FilenameMap.add relative_fn relative_file_data parsed_heaps

  (* Collect all the data for a single unparsed file *)
  let collect_normalized_data_for_unparsed_file ~root unparsed_heaps fn  =
    let relative_file_data = {
      unparsed_info = normalize_info ~root @@ Module_heaps.get_info_unsafe ~audit:Expensive.ok fn;
      unparsed_hash = Parsing_heaps.get_file_hash_unsafe fn;
    } in

    let relative_fn = normalize_file_key ~root fn in

    FilenameMap.add relative_fn relative_file_data unparsed_heaps

  (* The builtin flowlibs are excluded from the saved state. The server which loads the saved state
   * will extract and typecheck its own builtin flowlibs *)
  let is_not_in_flowlib ~options =
    match (Options.file_options options).Files.default_lib_dir with
    | None -> fun _ -> true (* There are no flowlibs *)
    | Some root ->
      let root_str = Path.to_string root in
      fun f -> not (Files.is_prefix root_str f)

  let normalize_error_set ~root error_set =
    let normalizer = new error_normalizer root in
    Errors.ErrorSet.map normalizer#error error_set

  (* Collect all the data for all the files *)
  let collect_data ~workers ~genv ~env =
    let options = genv.ServerEnv.options in
    let root = Options.root options |> Path.to_string in
    let%lwt parsed_heaps = MultiWorkerLwt.call workers
      ~job:(List.fold_left (collect_normalized_data_for_parsed_file ~root) )
      ~neutral:FilenameMap.empty
      ~merge:FilenameMap.union
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements env.ServerEnv.files))
    in
    let%lwt unparsed_heaps = MultiWorkerLwt.call workers
      ~job:(List.fold_left (collect_normalized_data_for_unparsed_file ~root) )
      ~neutral:FilenameMap.empty
      ~merge:FilenameMap.union
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements env.ServerEnv.unparsed))
    in
    let ordered_non_flowlib_libs =
      env.ServerEnv.ordered_libs
      |> List.filter (is_not_in_flowlib ~options)
      |> List.map (Files.relative_path root)
    in
    let local_errors = FilenameMap.fold (fun fn error_set acc ->
      let normalized_fn = normalize_file_key ~root fn in
      let normalized_error_set = normalize_error_set ~root error_set in
      FilenameMap.add normalized_fn normalized_error_set acc
    ) env.ServerEnv.errors.ServerEnv.local_errors FilenameMap.empty in
    let node_modules_containers =
      SSet.map (Files.relative_path root) !Files.node_modules_containers
    in
    let flowconfig_hash =
      FlowConfig.get_hash @@ Server_files_js.config_file (Options.flowconfig_name options)
      @@ Options.root options
    in
    Lwt.return {
      flowconfig_hash;
      parsed_heaps;
      unparsed_heaps;
      ordered_non_flowlib_libs;
      local_errors;
      node_modules_containers;
    }

  let save ~saved_state_filename ~genv ~env =
    Hh_logger.info "Collecting data for saved state";

    let workers = genv.ServerEnv.workers in

    let%lwt data = collect_data ~workers ~genv ~env in

    let filename = Path.to_string saved_state_filename in

    Hh_logger.info "Writing saved-state file at %S" filename;

    let%lwt fd = Lwt_unix.openfile filename [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    let%lwt header_bytes_written = write_version fd in

    let%lwt data_bytes_written =
      Marshal_tools_lwt.to_fd_with_preamble fd (data: saved_state_data)
    in
    let%lwt () = Lwt_unix.close fd in

    let bytes_written =
      header_bytes_written + Marshal_tools_lwt.expected_preamble_size + data_bytes_written
    in

    Hh_logger.info "Finished writing %d bytes to saved-state file at %S" bytes_written filename;

    Lwt.return_unit
end

exception Invalid_saved_state

(* Loading the saved state generally consists of 2 things:
 *
 * 1. Loading the saved state from a file
 * 2. Denormalizing the data. This generally means turning relative paths into absolute paths
 *
 * This is on the critical path for starting up a server with saved state. We really care about
 * the perf
 *)
module Load: sig
  val load:
    workers:MultiWorkerLwt.worker list option ->
    saved_state_filename:Path.t ->
    options:Options.t ->
    saved_state_data Lwt.t
end = struct

  let denormalize_file_key ~root fn = File_key.map (Files.absolute_path root) fn

  class file_sig_denormalizer (root) = object
    inherit File_sig.mapper

    method! loc (loc: Loc.t) =
      { loc with
        Loc.source = Option.map ~f:(denormalize_file_key ~root) loc.Loc.source;
      }
  end

  class error_denormalizer (root) = object
    inherit Errors.mapper

    method! loc (loc: Loc.t) =
      { loc with
        Loc.source = Option.map ~f:(denormalize_file_key ~root) loc.Loc.source;
      }
  end

  let verify_version =
    let version_length = String.length Flow_version.version in
    let rec read_version fd buf offset len =
      if len > 0
      then
        let%lwt bytes_read = Lwt_unix.read fd buf offset len in
        if bytes_read = 0
        then begin
          Hh_logger.error
            "Invalid saved state version header. It should be %d bytes but only read %d bytes"
            version_length
            (version_length - len);
          raise Invalid_saved_state
        end;
        let offset = offset + bytes_read in
        let len = len - bytes_read in
        read_version fd buf offset len
      else
        let result = Bytes.to_string buf in
        if result <> Flow_version.version
        then begin
          Hh_logger.error
            "Saved-state file failed version check. Expected version %S but got %S"
            Flow_version.version
            result;
          raise Invalid_saved_state
        end else Lwt.return_unit
    in
    fun fd -> read_version fd (Bytes.create version_length) 0 version_length

  let denormalize_info ~root info =
    let module_name =
      modulename_map_fn ~f:(denormalize_file_key ~root) info.Module_heaps.module_name
    in
    { info with Module_heaps.module_name }

  (* Turns all the relative paths in a file's data back into absolute paths.
   *
   * We do our best to avoid reading the file system (which Path.make will do) *)
  let denormalize_parsed_data ~root file_data =
    (* info *)
    let info = denormalize_info ~root file_data.info in

    (* file_sig *)
    let file_sig = (new file_sig_denormalizer root)#file_sig file_data.file_sig in

    (* resolved_requires *)
    let { Module_heaps.resolved_modules; phantom_dependents } = file_data.resolved_requires in
    let phantom_dependents = SSet.map (Files.absolute_path root) phantom_dependents in
    let resolved_modules = SMap.map
      (modulename_map_fn ~f:(denormalize_file_key ~root)) resolved_modules in
    let resolved_requires = { Module_heaps.resolved_modules; phantom_dependents } in

    {
      package = file_data.package;
      info;
      file_sig;
      resolved_requires;
      hash = file_data.hash;
    }

  let progress_fn real_total offset ~total:_ ~start ~length:_ =
    let finished = start + offset in
    MonitorRPC.status_update
      ServerStatus.(Load_saved_state_progress { total = Some real_total; finished })

  (* Denormalize the data for all the parsed files. This is kind of slow :( *)
  let denormalize_parsed_heaps ~workers ~root ~progress_fn parsed_heaps =
    let next =
      MultiWorkerLwt.next ~progress_fn ~max_size:4000 workers (FilenameMap.bindings parsed_heaps)
    in
    MultiWorkerLwt.call workers
      ~job:(List.fold_left (fun acc (relative_fn, parsed_file_data) ->
        let parsed_file_data = denormalize_parsed_data ~root parsed_file_data in
        let fn = denormalize_file_key ~root relative_fn in
        FilenameMap.add fn parsed_file_data acc
      ))
      ~neutral:FilenameMap.empty
      ~merge:FilenameMap.union
      ~next

  (* Denormalize the data for all the unparsed files *)
  let denormalize_unparsed_heaps ~workers ~root ~progress_fn unparsed_heaps =
    let next =
      MultiWorkerLwt.next ~progress_fn ~max_size:4000 workers (FilenameMap.bindings unparsed_heaps)
    in
    MultiWorkerLwt.call workers
      ~job:(List.fold_left (fun acc (relative_fn, unparsed_file_data) ->
        let unparsed_info = denormalize_info ~root unparsed_file_data.unparsed_info in
        let fn = denormalize_file_key ~root relative_fn in
        FilenameMap.add fn { unparsed_info; unparsed_hash = unparsed_file_data.unparsed_hash; } acc
      ))
      ~neutral:FilenameMap.empty
      ~merge:FilenameMap.union
      ~next

  let denormalize_error_set ~root normalized_error_set =
    let denormalizer = new error_denormalizer root in
    Errors.ErrorSet.map denormalizer#error normalized_error_set

  (* Denormalize all the data *)
  let denormalize_data ~workers ~options ~data =
    let root = Options.root options |> Path.to_string in

    let {
      flowconfig_hash;
      parsed_heaps;
      unparsed_heaps;
      ordered_non_flowlib_libs;
      local_errors;
      node_modules_containers;
    } = data in

    let current_flowconfig_hash =
      let flowconfig_name = Options.flowconfig_name options in
      FlowConfig.get_hash @@ Server_files_js.config_file flowconfig_name @@ Options.root options
    in

    if flowconfig_hash <> current_flowconfig_hash
    then begin
      Hh_logger.error
        "Invalid saved state: .flowconfig has changed since this saved state was generated.";
      raise Invalid_saved_state
    end;

    let parsed_count = FilenameMap.cardinal parsed_heaps in
    let progress_fn = progress_fn (parsed_count + (FilenameMap.cardinal unparsed_heaps)) in

    Hh_logger.info "Denormalizing the data for the parsed files";
    let%lwt parsed_heaps =
      let progress_fn = progress_fn 0 in
      denormalize_parsed_heaps ~workers ~root ~progress_fn parsed_heaps
    in

    Hh_logger.info "Denormalizing the data for the unparsed files";
    let%lwt unparsed_heaps =
      let progress_fn = progress_fn parsed_count in
      denormalize_unparsed_heaps ~workers ~root ~progress_fn unparsed_heaps
    in

    let ordered_non_flowlib_libs = List.map (Files.absolute_path root) ordered_non_flowlib_libs in

    let local_errors = FilenameMap.fold (fun normalized_fn normalized_error_set acc ->
      let fn = denormalize_file_key ~root normalized_fn in
      let error_set = denormalize_error_set ~root normalized_error_set in
      FilenameMap.add fn error_set acc
    ) local_errors FilenameMap.empty in

    let node_modules_containers = SSet.map (Files.absolute_path root) node_modules_containers in

    Lwt.return {
      flowconfig_hash;
      parsed_heaps;
      unparsed_heaps;
      ordered_non_flowlib_libs;
      local_errors;
      node_modules_containers;
    }

  let load ~workers ~saved_state_filename ~options =
    let filename = Path.to_string saved_state_filename in

    Hh_logger.info "Reading saved-state file at %S" filename;

    MonitorRPC.status_update ServerStatus.Read_saved_state;

    let%lwt fd = try%lwt
      Lwt_unix.openfile filename [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o666
    with
    | Unix.Unix_error(Unix.ENOENT, _, _) as exn ->
      Hh_logger.error ~exn "Failed to open %S" filename;
      raise Invalid_saved_state
    in

    let%lwt () = verify_version fd in
    let%lwt (data: saved_state_data) =
      try%lwt Marshal_tools_lwt.from_fd_with_preamble fd
      with exn ->
        Hh_logger.error ~exn "Failed to parsed saved state data";
        raise Invalid_saved_state
    in

    let%lwt () = Lwt_unix.close fd in

    Hh_logger.info "Denormalizing saved-state data";

    let%lwt data = denormalize_data ~workers ~options ~data in

    Hh_logger.info "Finished loading saved-state";

    Lwt.return data
end

let save = Save.save
let load = Load.load
