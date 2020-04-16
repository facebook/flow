(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Codemod_report
module FilenameMap = Utils_js.FilenameMap

type ('a, 't) abstract_codemod_runner =
  | Mapper of ('t -> 'a Codemod_ast_mapper.mapper)
  | Reducer of ('t -> 'a Codemod_ast_reducer.reducer)

type 'a codemod_runner =
  | TypedRunner of ('a, Codemod_context.Typed.t) abstract_codemod_runner
  | UntypedRunner of ('a, Codemod_context.Untyped.t) abstract_codemod_runner

type 'a job_config = {
  runner: 'a codemod_runner;
  reporter: 'a Codemod_report.t;
}

let print_rule =
  let line = String.make 80 '~' in
  (fun () -> Utils_js.print_endlinef "%s" line)

let print_ast_file_dry ~strip_root ~info file =
  let file_path = File_key.to_string file in
  let file_input = File_input.FileName file_path in
  match Diff_heaps.get_diff file with
  | Some (_x :: _xs as diff) ->
    let source = Replacement_printer.print_unsafe diff file_input in
    let file_path = Reason.string_of_source ~strip_root file in
    print_rule ();
    Utils_js.print_endlinef "%s" file_path;
    Utils_js.print_endlinef "Number of changes: %d" (List.length diff);
    print_rule ();
    Utils_js.print_endlinef "%s" source
  | Some []
  | None ->
    if info then (
      let file_path = Reason.string_of_source ~strip_root file in
      print_rule ();
      Utils_js.print_endlinef "%s" file_path;
      Utils_js.print_endlinef "No changes";
      print_rule ()
    )

let print_ast_file_real ~info file =
  let file_path = File_key.to_string file in
  let file_input = File_input.FileName file_path in
  match Diff_heaps.get_diff file with
  | Some (_x :: _xs as diff) ->
    let source = Replacement_printer.print_unsafe diff file_input in
    if info then Hh_logger.info "Applying changes to %s" file_path;
    let%lwt chan = Lwt_io.open_file ~mode:Lwt_io.output file_path in
    let%lwt () = Lwt_io.fprint chan source in
    let%lwt () = Lwt_io.close chan in
    Lwt.return (Some file)
  | Some []
  | None ->
    Lwt.return None

let max_files_open = 1024

(* Returns None Lwt.t if called in dry_run mode. Otherwise, returns (Some list) Lwt.t
   where list contains the files that were changed. *)
let print_asts ~strip_root ~info ~dry_run files : File_key.t list option Lwt.t =
  let print_dry () =
    List.iter (print_ast_file_dry ~strip_root ~info) files;
    Lwt.return None
  in
  let print_real () =
    let buckets = ListUtils.bucket_n max_files_open files in
    let%lwt changed_files =
      Lwt_list.fold_left_s
        (fun acc files ->
          let%lwt changed_files = Lwt_list.filter_map_p (print_ast_file_real ~info) files in
          Lwt.return (List.rev_append changed_files acc))
        []
        buckets
    in
    Lwt.return (Some changed_files)
  in
  if dry_run then
    print_dry ()
  else
    print_real ()

let print_results ~report result : unit =
  print_rule ();
  Utils_js.print_endlinef "Launching report";
  print_rule ();
  report result

let digest_typed_results ~reporter results =
  FilenameMap.fold
    (fun file_key result acc ->
      match result with
      | Ok ok ->
        let (acc_files, acc_result) = acc in
        (file_key :: acc_files, reporter.combine acc_result ok)
      | Error (aloc, err) ->
        Utils_js.prerr_endlinef
          "%s: %s"
          (Reason.string_of_aloc aloc)
          (Error_message.string_of_internal_error err);
        acc)
    results
    ([], reporter.empty)

let digest_untyped_results ~reporter results =
  FilenameMap.fold
    (fun file_key r acc ->
      match r with
      | None -> acc
      | Some result ->
        let (acc_files, acc_result) = acc in
        (file_key :: acc_files, reporter.Codemod_report.combine result acc_result))
    results
    ([], reporter.empty)

let initialize_logs options = LoggingUtils.init_loggers ~options ()

let mk_main
    (job_config : 'a job_config)
    ~options
    ~info
    ~verbose
    ~dry_run
    ~log_level
    ~shared_mem_config
    roots =
  initialize_logs options;
  let log_level =
    if verbose then
      Hh_logger.Level.Debug
    else
      match log_level with
      | Some level -> level
      | None -> Hh_logger.Level.Off
  in
  Hh_logger.Level.set_min_level log_level;

  (* Mappers produce new ASTs, which are saved to the heap. *)
  let save_ast_diff file_key ast ast' =
    let diff = Flow_ast_differ.program Flow_ast_differ.Standard ast ast' in
    if List.length diff = 0 then
      ()
    else
      let file_path = File_key.to_string file_key in
      let file_input = File_input.FileName file_path in
      let patch = Replacement_printer.mk_patch_ast_differ_unsafe diff file_input in
      if info then
        Hh_logger.print_with_newline
          "patches for %s:\n%s"
          file_path
          (Replacement_printer.show_patch patch);
      Diff_heaps.set_diff ~audit:Expensive.ok file_key patch
  in

  let f
        : 't 'a. 't -> File_key.t -> (Loc.t, Loc.t) Flow_ast.program ->
          ('a, 't) abstract_codemod_runner -> 'a =
   fun cctx file_key ast runner ->
    match runner with
    | Reducer reducer ->
      let reducer = reducer cctx in
      let (_ : (Loc.t, Loc.t) Flow_ast.program) = reducer#program ast in
      reducer#acc
    | Mapper mapper ->
      let mapper = mapper cctx in
      let ast' = mapper#program ast in
      save_ast_diff file_key ast ast';
      mapper#acc
  in
  let initial_lwt_thread () =
    let genv =
      let num_workers = Options.max_workers options in
      let handle = SharedMem_js.init ~num_workers shared_mem_config in
      ServerEnvBuild.make_genv options handle
    in
    let should_print_summary = Options.should_profile options in

    let strip_root =
      if Options.should_strip_root options then
        Some (Options.root options)
      else
        None
    in
    let%lwt (files, result) =
      match job_config.runner with
      | TypedRunner config ->
        let f ast typed_context =
          let { Codemod_context.Typed.file; _ } = typed_context in
          f typed_context file ast config
        in
        let%lwt (_, results) =
          Codemod_runner.TypedRunner.run ~genv ~should_print_summary ~info ~f options roots
        in
        let results = digest_typed_results ~reporter:job_config.reporter results in
        Lwt.return results
      | UntypedRunner config ->
        let f ast untyped_context =
          let { Codemod_context.Untyped.file; _ } = untyped_context in
          f untyped_context file ast config
        in
        let%lwt (_, results) =
          Codemod_runner.UntypedRunner.run ~genv ~should_print_summary ~f options roots
        in
        let results = digest_untyped_results ~reporter:job_config.reporter results in
        Lwt.return results
    in
    Hh_logger.info "Applying results";
    let%lwt _changed_files = print_asts ~strip_root ~info ~dry_run files in
    print_results ~report:(job_config.reporter.Codemod_report.report ~strip_root) result;
    Lwt.return ()
  in
  LwtInit.run_lwt initial_lwt_thread
