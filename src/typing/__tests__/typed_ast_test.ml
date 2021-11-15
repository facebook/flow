(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module File_sig = File_sig.With_Loc
module Env = Env.Env
include Type_inference_js.Make_Inference (Env)

(* pretty much copied from Flow_dot_js *)
let metadata =
  {
    Context.checked (* local *) = true;
    munge_underscores = false;
    verbose = None;
    weak = false;
    jsx = Options.Jsx_react;
    strict = false;
    strict_local = false;
    include_suppressions = false;
    (* global *)
    automatic_require_default = false;
    babel_loose_array_spread = false;
    max_literal_length = 100;
    enable_const_params = false;
    enable_enums = true;
    enable_relay_integration = false;
    env_mode = Options.ClassicEnv [];
    env_mode_constrain_write_dirs = [];
    enforce_strict_call_arity = true;
    enforce_local_inference_annotations = false;
    local_inference_annotation_dirs = [];
    enforce_this_annotations = false;
    experimental_infer_indexers = false;
    exact_by_default = false;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    haste_module_ref_prefix = None;
    ignore_non_literal_requires = false;
    max_trace_depth = 0;
    max_workers = 0;
    react_runtime = Options.ReactRuntimeClassic;
    react_server_component_exts = SSet.empty;
    recursion_limit = 10000;
    relay_integration_module_prefix = None;
    reorder_checking = Options.Lexical;
    root = Path.dummy_path;
    run_post_inference_implicit_instantiation = false;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
    type_asserts = false;
  }

(* somewhat copied from Flow_dot_js *)
let parse_content file content =
  let parse_options =
    Some
      Parser_env.
        {
          enums = true;
          esproposal_class_instance_fields = true;
          esproposal_class_static_fields = true;
          esproposal_decorators = true;
          esproposal_export_star_as = true;
          esproposal_optional_chaining = true;
          esproposal_nullish_coalescing = true;
          types = true;
          use_strict = false;
        }
      
  in

  let (ast, _parse_errors) =
    Parser_flow.program_file ~fail:false ~parse_options content (Some file)
  in
  match File_sig.program ~ast ~opts:File_sig.default_opts with
  | Ok (fsig, _) -> Ok (ast, fsig)
  | Error e -> Error e

(* copied from Type_inference_js *)
(* TODO: consider whether require tvars are necessary, and if not, take this out *)
let add_require_tvars =
  let add cx desc loc =
    let loc = ALoc.of_loc loc in
    let reason = Reason.mk_reason desc loc in
    let id = Tvar.mk_no_wrap cx reason in
    Context.add_require cx loc (reason, id)
  in
  let add_decl cx m_name desc loc =
    (* TODO: Imports within `declare module`s can only reference other `declare
       module`s (for now). This won't fly forever so at some point we'll need to
       move `declare module` storage into the modulemap just like normal modules
       and merge them as such. *)
    let loc = ALoc.of_loc loc in
    let reason = Reason.mk_reason desc loc in
    let tvar = Flow_js.get_builtin_tvar cx m_name reason in
    Context.add_require cx loc (reason, tvar)
  in
  fun cx file_sig ->
    File_sig.(
      SMap.iter
        (fun mref locs ->
          let desc = Reason.RCustom mref in
          Nel.iter (add cx desc) locs)
        (require_loc_map file_sig.module_sig);
      SMap.iter
        (fun _ (_, module_sig) ->
          SMap.iter
            (fun mref locs ->
              let m_name = Reason.internal_module_name mref in
              let desc = Reason.RCustom mref in
              Nel.iter (add_decl cx m_name desc) locs)
            (require_loc_map module_sig))
        file_sig.declare_modules
    )

let before_and_after_stmts file_name =
  let content = Sys_utils.cat file_name in
  let file_key = File_key.LibFile file_name in
  match parse_content file_key content with
  | Error e -> Error e
  | Ok ((_, { Flow_ast.Program.statements = stmts; _ }), file_sig) ->
    (* Loading the entire libdefs here would be overkill, but the typed_ast tests do use Object
     * in a few tests. In order to avoid EBuiltinLookupFailed errors with an empty source location,
     * we manually add "Object" -> Any into the builtins map. We use the UnresolvedName any type
     * to avoid any "Any value used as type" errors that may otherwise appear *)
    let master_cx = Context.empty_master_cx () in
    let () =
      let reason =
        let loc = ALoc.none in
        let desc = Reason.RCustom "Explicit any used in type_ast tests" in
        Reason.mk_reason desc loc
      in
      Builtins.set_builtin
        ~flow_t:(fun _ -> ())
        master_cx.Context.builtins
        (Reason.OrdinaryName "Object")
        (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)))
    in
    let cx =
      let aloc_table = lazy (ALoc.empty_table file_key) in
      let ccx = Context.(make_ccx master_cx) in
      Context.make
        ccx
        metadata
        file_key
        aloc_table
        (Reason.OrdinaryName Files.lib_module_ref)
        Context.Checking
    in
    add_require_tvars cx file_sig;
    let module_scope = Scope.fresh () in
    Env.init_env cx module_scope;
    let stmts = Base.List.map ~f:Ast_loc_utils.loc_to_aloc_mapper#statement stmts in
    let t_stmts =
      try
        Statement.toplevel_decls cx stmts;
        Statement.Toplevels.toplevels Statement.statement cx stmts
      with
      | Abnormal.Exn (Abnormal.Stmts t_stmts, _) -> t_stmts
      | Abnormal.Exn (Abnormal.Stmt t_stmt, _) -> [t_stmt]
      | Abnormal.Exn (Abnormal.Expr (annot, t_expr), _) ->
        [
          ( annot,
            Flow_ast.Statement.Expression
              {
                Flow_ast.Statement.Expression.expression = t_expr;
                directive = None;
                comments = None;
              }
          );
        ]
      | e ->
        let e = Exception.wrap e in
        let message = Exception.get_ctor_string e in
        let stack = Exception.get_backtrace_string e in
        assert_failure (Utils_js.spf "Exception: %s\nStack:\n%s\n" message stack)
    in
    Ok (stmts, t_stmts)

class ['a, 'b] loc_none_mapper =
  object
    inherit ['a, 'b, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot (_x : 'a) = Loc.none

    method on_type_annot (_x : 'b) = Loc.none
  end

class aloc_mapper =
  object
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = x

    method on_type_annot (x, _) = x
  end

let diff_dir =
  let tmp_dir = FlowConfig.temp_dir FlowConfig.empty_config in
  Random.self_init ();
  let extension = Printf.sprintf "typed_ast_test_%d" (Random.int 0x3FFFFFFF) in
  Filename.concat tmp_dir extension

let system_diff ~f prefix =
  let dump_stmts filename stmts =
    let stmts = f stmts in
    let stmts_file = Path.to_string (Path.concat (Path.make diff_dir) filename) in
    let oc = open_out stmts_file in
    output_string oc stmts;
    close_out oc;
    stmts_file
  in
  fun stmts1 stmts2 ->
    let result =
      try
        Disk.mkdir_p diff_dir;
        let stmts1_file = dump_stmts (prefix ^ "_A.js") stmts1 in
        let stmts2_file = dump_stmts (prefix ^ "_B.js") stmts2 in
        let out_file = prefix ^ "_diff.txt" |> Path.concat (Path.make diff_dir) |> Path.to_string in
        let cmd = Utils_js.spf "diff -U7 %s %s > %s" stmts1_file stmts2_file out_file in
        match Sys.command cmd with
        | 0
        | 1 ->
          let chan = open_in out_file in
          let s = Sys_utils.read_all chan in
          Utils_js.print_endlinef "READ: %s" s;
          close_in chan;
          Ok s
        | code ->
          Utils_js.print_endlinef "diff read error code %d" code;
          Error "diff wasn't able to run for some reason"
      with
      | e ->
        let e = Exception.wrap e in
        let msg = Exception.get_ctor_string e in
        Error msg
    in
    Disk.rm_dir_tree diff_dir;
    match result with
    | Ok diff -> diff
    | Error msg -> failwith msg

let pp_diff =
  let aloc_pp fmt x = Loc.pp fmt (ALoc.to_loc_exn x) in
  let string_of_ast stmts =
    Base.List.map ~f:(Flow_ast.Statement.show aloc_pp aloc_pp) stmts |> String.concat "\n"
  in
  let string_of_src stmts =
    let none_mapper = new loc_none_mapper in
    let prog =
      ( Loc.none,
        {
          Flow_ast.Program.statements = Base.List.map ~f:none_mapper#statement stmts;
          comments = None;
          all_comments = [];
        }
      )
    in
    let layout = Js_layout_generator.program ~preserve_docblock:false ~checksum:None prog in
    layout |> Pretty_printer.print ~source_maps:None |> Source.contents
  in
  fun fmt (stmts1, stmts2) ->
    let ast_diff = system_diff ~f:string_of_ast "ast" stmts1 stmts2 in
    let src_diff = system_diff ~f:string_of_src "src" stmts1 stmts2 in
    Format.pp_print_string
      fmt
      ("\n" ^ "AST tree diff:\n" ^ ast_diff ^ "\n\n" ^ "Source diff:\n" ^ src_diff)

let check_structural_equality relative_path file_name stmts1 stmts2 =
  let aloc_mapper = new aloc_mapper in
  let stmts2 = aloc_mapper#toplevel_statement_list stmts2 in
  let path =
    match Sys_utils.realpath file_name with
    | Some path -> path
    | None -> relative_path
  in
  let msg =
    path
    ^ ":\n"
    ^ "The structure of the produced Typed AST differs from that of the parsed AST.\n\n"
    ^ "To fix this do one of the following:\n"
    ^ " * restore the produced Typed AST, or\n"
    ^ " * include \""
    ^ relative_path
    ^ "\" in the blocklist section\n"
    ^ "   in src/typing/__tests__/typed_ast_test.ml and file a task with the\n"
    ^ "   'flow-typed-ast' tag.\n"
  in
  assert_equal ~pp_diff ~msg stmts1 stmts2

let test_case relative_path file_name _ =
  match before_and_after_stmts file_name with
  | Ok (s, s') -> check_structural_equality relative_path file_name s s'
  | Error (File_sig.IndeterminateModuleType _) -> ()

(* This list includes files for which the produced Typed AST differs in structure
 * from the parsed AST. *)
let blocklist = SSet.of_list ["invariant_reachability/index.js"; "return/implicit_void.js"]

let tests =
  let relative_test_dir = "flow/tests" in
  let root = Base.Option.value_exn (Sys_utils.realpath relative_test_dir) in
  let files = CommandUtils.expand_file_list [relative_test_dir] in
  let tests =
    let slash_regex = Str.regexp_string "/" in
    SSet.fold
      (fun file acc ->
        let relative_path = Files.relative_path root file in
        if SSet.mem relative_path blocklist then
          acc
        else
          let test_name =
            relative_path |> Str.global_replace slash_regex "_" |> Filename.chop_extension
          in
          (test_name >:: test_case relative_path (relative_test_dir ^ "/" ^ relative_path)) :: acc)
      files
      []
  in
  "TypedAST" >::: tests
