(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameMap = Utils_js.FilenameMap

type ('a, 't) abstract_codemod_runner =
  | Mapper of ('t -> 'a Codemod_ast_mapper.mapper)
  | Reducer of ('t -> 'a Codemod_ast_reducer.reducer)

type 'a codemod_runner =
  | TypedRunner of ('a, Codemod_context.Typed.t) abstract_codemod_runner
  | UntypedFlowInitRunner of {
      init: reader:State_reader.t -> unit;
      runner: ('a, Codemod_context.UntypedFlowInit.t) abstract_codemod_runner;
    }
  | UntypedRunner of ('a, Codemod_context.Untyped.t) abstract_codemod_runner

type 'a job_config = {
  runner: 'a codemod_runner;
  reporter: 'a Codemod_report.t;
}

(* Mappers produce new ASTs, which are saved to the heap. *)
let save_ast_diff file_key ast ast' =
  let diff = Flow_ast_differ.program Flow_ast_differ.Standard ast ast' in
  if List.length diff = 0 then
    ()
  else
    let file_path = File_key.to_string file_key in
    let file_input = File_input.FileName file_path in
    let layout_opts = { Js_layout_generator.preserve_formatting = true; bracket_spacing = false } in
    let patch = Replacement_printer.mk_patch_ast_differ_unsafe ~opts:layout_opts diff file_input in
    Diff_heaps.set_diff ~audit:Expensive.ok file_key patch

let make_visitor job_config =
  let f ask file_key ast runner =
    match runner with
    | Reducer reducer ->
      let reducer = reducer ask in
      let (_ : (Loc.t, Loc.t) Flow_ast.Program.t) = reducer#program ast in
      reducer#acc
    | Mapper mapper ->
      let mapper = mapper ask in
      let ast' = mapper#program ast in
      save_ast_diff file_key ast ast';
      mapper#acc
  in
  match job_config.runner with
  | TypedRunner runner ->
    let f ast typed_ask =
      let { Codemod_context.Typed.file; _ } = typed_ask in
      f typed_ask file ast runner
    in
    Codemod_runner.Typed_visitor f
  | UntypedRunner runner ->
    let f ast untyped_ask =
      let { Codemod_context.Untyped.file; _ } = untyped_ask in
      f untyped_ask file ast runner
    in
    Codemod_runner.Untyped_visitor f
  | UntypedFlowInitRunner { runner; init } ->
    let f ast untyped_ask =
      let { Codemod_context.UntypedFlowInit.file; _ } = untyped_ask in
      f untyped_ask file ast runner
    in
    Codemod_runner.UntypedFlowInitRunner_visitor { Codemod_runner.visit = f; init }

let initialize_logs options = LoggingUtils.init_loggers ~options ()

let mk_main job_config ~options ~write ~repeat ~log_level ~shared_mem_config roots =
  let init_id = Random_id.short_string () in
  initialize_logs options;
  let log_level =
    match log_level with
    | Some level -> level
    | None -> Hh_logger.Level.Off
  in
  Hh_logger.Level.set_min_level log_level;

  let initial_lwt_thread () =
    let genv =
      let num_workers = Options.max_workers options in
      let handle = SharedMem_js.init ~num_workers shared_mem_config in
      ServerEnvBuild.make_genv ~init_id ~options handle
    in
    let visitor = make_visitor job_config in
    let reporter = job_config.reporter in
    Codemod_runner.run ~genv ~write ~repeat ~visitor ~reporter roots
  in
  LwtInit.run_lwt initial_lwt_thread
