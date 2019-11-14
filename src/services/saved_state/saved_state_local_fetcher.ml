(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This saved state fetcher is intended to be used mainly by tests. It assumes that there are 2
 * files next to /path/to/root/.flowconfig:
 *
 * /path/to/root/.flow.saved_state - the saved state file
 * /path/to/root/.flow.saved_state_file_changes - newline separated list of files which have changed
 *
 * if either file doesn't exist then we assume there's no saved state
 *)
include (
  struct
    let fetch ~options =
      Profiling_js.with_profiling_lwt ~label:"FetchSavedState" ~should_print_summary:false (fun _ ->
          let root_str = Options.root options |> Path.to_string in
          let saved_state_file = Filename.concat root_str ".flow.saved_state" in
          let changed_files_input_file =
            Filename.concat root_str ".flow.saved_state_file_changes"
          in
          let%lwt saved_state_exists = Lwt_unix.file_exists saved_state_file
          and input_file_exists = Lwt_unix.file_exists changed_files_input_file in
          if saved_state_exists && input_file_exists then
            let changed_files =
              Sys_utils.lines_of_file changed_files_input_file
              |> Files.canonicalize_filenames
                   ~handle_imaginary:Files.imaginary_realpath
                   ~cwd:root_str
              |> SSet.of_list
            in
            Lwt.return
              (Saved_state_fetcher.Saved_state
                 { saved_state_filename = Path.make saved_state_file; changed_files })
          else (
            if not saved_state_exists then Hh_logger.error "File %S does not exist" saved_state_file;
            if not input_file_exists then
              Hh_logger.error "File %S does not exist" changed_files_input_file;

            Lwt.return Saved_state_fetcher.No_saved_state
          ))
  end :
    Saved_state_fetcher.FETCHER )
