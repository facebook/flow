(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameSet = Utils_js.FilenameSet

let spf = Printf.sprintf

type error = {
  msg: string;
  exit_status: FlowExitStatus.t;
}

let is_incompatible_package_json ~options ~reader =
  (* WARNING! Be careful when adding new incompatibilities to this function. While dfind will
   * return any file which changes within the watched directories, watchman only watches for
   * specific extensions and files. Make sure to update the watchman_expression_terms in our
   * watchman file watcher! *)
  let is_incompatible filename_str =
    let filename = File_key.JsonFile filename_str in
    match Sys_utils.cat_or_failed filename_str with
    | None -> true (* Failed to read package.json *)
    | Some content ->
      (try
         let ast = Parsing_service_js.parse_json_file ~fail:true content filename in
         Module_js.package_incompatible ~options ~reader filename_str ast
       with _ -> true)
    (* Failed to parse package.json *)
  in
  fun ~want ~sroot ~file_options f ->
    (String_utils.string_starts_with f sroot || Files.is_included file_options f)
    && Filename.basename f = "package.json"
    && want f
    && is_incompatible f

(* This function takes a set of filenames. We have been told that these files have changed. The
 * main job of this function is to tell
 *
 * 1. Do we care about this file? Maybe the file is ignored or has the wrong extension
 * 2. If we do care, are we unable to incrementally check this change. For example, maybe a libdef
 *    changed or the .flowconfig changed. Maybe one day we'll learn to incrementally check those
 *    changes, but for now we just need to exit and restart from scratch *)
let process_updates ?(skip_incompatible = false) ~options ~libs updates =
  Core_result.(
    let reader = State_reader.create () in
    let file_options = Options.file_options options in
    let all_libs =
      let known_libs = libs in
      let (_, maybe_new_libs) = Files.init file_options in
      SSet.union known_libs maybe_new_libs
    in
    let root = Options.root options in
    let config_path = Server_files_js.config_file (Options.flowconfig_name options) root in
    let sroot = Path.to_string root in
    let want = Files.wanted ~options:file_options all_libs in
    Ok ()
    >>= fun () ->
    (* Die if the .flowconfig changed *)
    if (not skip_incompatible) && SSet.mem config_path updates then
      Error
        {
          msg = spf "%s changed in an incompatible way. Exiting." config_path;
          exit_status = FlowExitStatus.Flowconfig_changed;
        }
    else
      Ok ()
      >>= fun () ->
      let is_incompatible_package_json =
        is_incompatible_package_json ~options ~reader ~want ~sroot ~file_options
      in
      (* Die if a package.json changed in an incompatible way *)
      let incompatible_packages = SSet.filter is_incompatible_package_json updates in
      if (not skip_incompatible) && not (SSet.is_empty incompatible_packages) then
        let messages =
          SSet.elements incompatible_packages
          |> List.rev_map (spf "Modified package: %s")
          |> String.concat "\n"
        in
        Error
          {
            msg = spf "%s\nPackages changed in an incompatible way. Exiting." messages;
            exit_status = FlowExitStatus.Server_out_of_date;
          }
      else
        Ok ()
        >>= fun () ->
        let flow_typed_path = Path.to_string (Files.get_flowtyped_path root) in
        let is_changed_lib filename =
          let is_lib = SSet.mem filename all_libs || filename = flow_typed_path in
          is_lib
          &&
          let file = File_key.LibFile filename in
          match Sys_utils.cat_or_failed filename with
          | None -> true (* Failed to read lib file *)
          | Some content ->
            (* Check if the lib file's hash has changed *)
            not (Parsing_service_js.does_content_match_file_hash ~reader file content)
        in
        (* Die if a lib file changed *)
        let libs = updates |> SSet.filter is_changed_lib in
        if (not skip_incompatible) && not (SSet.is_empty libs) then
          let messages =
            SSet.elements libs |> List.rev_map (spf "Modified lib file: %s") |> String.concat "\n"
          in
          Error
            {
              msg = spf "%s\nLib files changed in an incompatible way. Exiting" messages;
              exit_status = FlowExitStatus.Server_out_of_date;
            }
        else
          Ok ()
          >>= fun () ->
          let is_flow_file = Files.is_flow_file ~options:file_options in
          Ok
            (SSet.fold
               (fun f acc ->
                 if
                   is_flow_file f
                   (* note: is_included may be expensive. check in-root match first. *)
                   && (String_utils.string_starts_with f sroot || Files.is_included file_options f)
                   && (* removes excluded and lib files. the latter are already filtered *)
                      want f
                 then
                   let filename = Files.filename_from_string ~options:file_options f in
                   FilenameSet.add filename acc
                 else
                   acc)
               updates
               FilenameSet.empty))
