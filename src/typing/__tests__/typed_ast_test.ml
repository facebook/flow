(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

module File_sig = File_sig.With_Loc

(* pretty much copied from Flow_dot_js *)
let metadata = { Context.
  (* local *)
  checked = true;
  munge_underscores = false;
  verbose = None;
  weak = false;
  jsx = Options.Jsx_react;
  strict = false;
  strict_local = false;

  (* global *)
  max_literal_length = 100;
  enable_const_params = false;
  enforce_strict_call_arity = true;
  esproposal_class_static_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_class_instance_fields = Options.ESPROPOSAL_ENABLE;
  esproposal_decorators = Options.ESPROPOSAL_ENABLE;
  esproposal_export_star_as = Options.ESPROPOSAL_ENABLE;
  esproposal_optional_chaining = Options.ESPROPOSAL_ENABLE;
  esproposal_nullish_coalescing = Options.ESPROPOSAL_ENABLE;
  facebook_fbs = None;
  facebook_fbt = None;
  haste_module_ref_prefix = None;
  ignore_non_literal_requires = false;
  max_trace_depth = 0;
  max_workers = 0;
  root = Path.dummy_path;
  strip_root = true;
  suppress_comments = [];
  suppress_types = SSet.empty;
}

(* somewhat copied from Flow_dot_js *)
let parse_content file content =
  let parse_options = Some Parser_env.({
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    esproposal_optional_chaining = true;
    esproposal_nullish_coalescing = true;
    types = true;
    use_strict = false;
  }) in
  let ast, _parse_errors =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  match File_sig.program ~ast ~module_ref_prefix:None with
  | Ok fsig -> ast, fsig
  | Error _ -> assert_failure "File_sig.program failed"

(* copied from Type_inference_js *)
(* TODO: consider whether require tvars are necessary, and if not, take this out *)
let add_require_tvars =
  let add cx desc loc =
    let loc = ALoc.of_loc loc in
    let reason = Reason.mk_reason desc loc in
    let t = Tvar.mk cx reason in
    Context.add_require cx loc t
  in
  let add_decl cx m_name desc loc =
    (* TODO: Imports within `declare module`s can only reference other `declare
       module`s (for now). This won't fly forever so at some point we'll need to
       move `declare module` storage into the modulemap just like normal modules
       and merge them as such. *)
    let loc = ALoc.of_loc loc in
    let reason = Reason.mk_reason desc loc in
    let t = Flow_js.get_builtin cx m_name reason in
    Context.add_require cx loc t
  in
  fun cx file_sig ->
    let open File_sig in
    SMap.iter (fun mref locs ->
      let desc = Reason.RCustom mref in
      Nel.iter (add cx desc) locs
    ) (require_loc_map file_sig.module_sig);
    SMap.iter (fun _ (_, module_sig) ->
      SMap.iter (fun mref locs ->
        let m_name = Reason.internal_module_name mref in
        let desc = Reason.RCustom mref in
        Nel.iter (add_decl cx m_name desc) locs
      ) (require_loc_map module_sig)
    ) file_sig.declare_modules

let lib_before_and_after_stmts file_name =
  let content = Sys_utils.cat file_name in
  let file_key = File_key.LibFile file_name in
  let (_, stmts, _), file_sig = parse_content file_key content in
  let cx =
    let sig_cx = Context.make_sig () in
    Context.make sig_cx metadata file_key Files.lib_module_ref
  in
  Flow_js.mk_builtins cx;
  Flow_js.Cache.clear ();
  add_require_tvars cx file_sig;
  let module_scope = Scope.fresh () in
  Env.init_env cx module_scope;
  let stmts = Core_list.map ~f:Ast_loc_utils.abstractify_mapper#statement stmts in
  let t_stmts =
    try
      Statement.toplevel_decls cx stmts;
      Statement.toplevels cx stmts
    with
    | Abnormal.Exn (Abnormal.Stmts t_stmts, Abnormal.Throw) -> t_stmts
    | _ -> assert_failure "constraint generation raised unexpected exception"
  in
  stmts, t_stmts


class ['a, 'b] loc_none_mapper = object(_)
  inherit ['a, 'b, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper
  method on_loc_annot (_x: 'a) = Loc.none
  method on_type_annot (_x: 'b) = Loc.none
end

let generate_stmts_layout (stmts: ('a, 'b) Flow_ast.Statement.t list) =
  let none_mapper = new loc_none_mapper in
  let prog = Loc.none, Core_list.map ~f:none_mapper#statement stmts, [] in
  let layout = Js_layout_generator.program ~preserve_docblock:false ~checksum:None prog in
  layout |> Pretty_printer.print ~source_maps:None |> Source.contents

let diff_dir =
  let flowconfig_name = Server_files_js.default_flowconfig_name in
  let tmp_dir = FlowConfig.temp_dir FlowConfig.empty_config in
  let root = CommandUtils.guess_root flowconfig_name (Some "flow/tests") in
  Random.self_init ();
  let extension = Printf.sprintf "typed_ast_test_%d" (Random.int 0x3FFFFFFF) in
  Server_files_js.file_of_root extension ~flowconfig_name ~tmp_dir root

let check_structural_equality (stmts1, stmts2) =
  let diff_output : int option ref = ref None in
  let err : exn option ref = ref None in
  begin try
    Disk.mkdir_p diff_dir;
    let stmts1_file = Path.to_string (Path.concat (Path.make diff_dir) "A.js") in
    let oc1 = open_out stmts1_file in
    output_string oc1 (generate_stmts_layout stmts1);
    close_out oc1;
    let stmts2_file = Path.to_string (Path.concat (Path.make diff_dir) "B.js") in
    let oc2 = open_out stmts2_file in
    output_string oc2 (generate_stmts_layout stmts2);
    close_out oc2;
    diff_output := Some (Sys.command (Printf.sprintf "diff %s %s" stmts1_file stmts2_file))
  with e ->
    err := Some e;
  end;
  Disk.rm_dir_tree diff_dir;
  Option.iter ~f:raise (!err);
  begin match !diff_output with
  | None -> assert_failure "diff wasn't able to run for some reason"
  | Some 0 -> ()
  | Some _ -> assert_failure "ASTs are different."
  end

let test_case file_name _ =
  file_name |> lib_before_and_after_stmts |> check_structural_equality

let tests = "TypedAST" >::: [
  "lib_serviceworkers" >:: test_case "flow/lib/serviceworkers.js"
]
