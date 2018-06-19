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
  info: Module_js.info;
  file_sig: File_sig.t;
  resolved_requires: Module_js.resolved_requires;
}

(* We also need to store the info for unparsed files *)
type unparsed_file_data = {
  unparsed_info: Module_js.info;
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
      modulename_map_fn ~f:(normalize_file_key ~root) info.Module_js.module_name
    in
    { info with Module_js.module_name }

  let normalize_parsed_data ~root parsed_file_data =
    (* info *)
    let info = normalize_info ~root parsed_file_data.info in

    (* file_sig *)
    let file_sig = (new file_sig_normalizer root)#file_sig parsed_file_data.file_sig in

    (* resolved_requires *)
    let { Module_js.resolved_modules; phantom_dependents } = parsed_file_data.resolved_requires in
    let phantom_dependents = SSet.map (Files.relative_path root) phantom_dependents in
    let resolved_modules = SMap.map
      (modulename_map_fn ~f:(normalize_file_key ~root)) resolved_modules in
    let resolved_requires = { Module_js.resolved_modules; phantom_dependents } in

    { package = parsed_file_data.package; info; file_sig; resolved_requires }

  (* Collect all the data for a single parsed file *)
  let collect_normalized_data_for_parsed_file ~root parsed_heaps fn =
    let package =
      match fn with
      | File_key.JsonFile str when Filename.basename str = "package.json" ->
          Some (Module_js.get_package_json_for_saved_state_unsafe str)
      | _ -> None
    in

    let file_data = {
      package;
      info = Module_js.get_info_unsafe ~audit:Expensive.ok fn;
      file_sig = Parsing_service_js.get_file_sig_unsafe fn;
      resolved_requires = Module_js.get_resolved_requires_unsafe ~audit:Expensive.ok fn;
    } in

    let relative_fn = normalize_file_key ~root fn in

    let relative_file_data = normalize_parsed_data ~root file_data in

    FilenameMap.add relative_fn relative_file_data parsed_heaps

  (* Collect all the data for a single unparsed file *)
  let collect_normalized_data_for_unparsed_file ~root unparsed_heaps fn  =
    let relative_file_data = {
      unparsed_info = normalize_info ~root @@ Module_js.get_info_unsafe ~audit:Expensive.ok fn;
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
      FlowConfig.get_hash @@ Server_files_js.config_file @@ Options.root options
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
let save = Save.save
