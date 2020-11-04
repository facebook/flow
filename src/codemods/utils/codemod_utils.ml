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
    let layout_opts =
      Js_layout_generator.
        {
          preserve_formatting = true;
          bracket_spacing = false;
          trailing_commas = Trailing_commas.All;
        }
    in
    let patch = Replacement_printer.mk_patch_ast_differ_unsafe ~opts:layout_opts diff file_input in
    Diff_heaps.set_diff ~audit:Expensive.ok file_key patch

let make_visitor :
    ('b, 'a) abstract_codemod_runner -> (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t -> 'a -> 'b =
 fun runner ast cctx ->
  let (prog_loc, _) = ast in
  let file = Base.Option.value_exn ~message:"No source for AST" (Loc.source prog_loc) in
  match runner with
  | Reducer reducer ->
    let reducer = reducer cctx in
    let (_ : (Loc.t, Loc.t) Flow_ast.Program.t) = reducer#program ast in
    reducer#acc
  | Mapper mapper ->
    let mapper = mapper cctx in
    let ast' = mapper#program ast in
    save_ast_diff file ast ast';
    mapper#acc

let initialize_logs options = LoggingUtils.init_loggers ~options ()

module MakeMain (Runner : Codemod_runner.RUNNABLE) = struct
  let main ~options ~write ~repeat ~log_level ~shared_mem_config roots =
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
      Runner.run ~genv ~write ~repeat roots
    in
    LwtInit.run_lwt initial_lwt_thread
end
