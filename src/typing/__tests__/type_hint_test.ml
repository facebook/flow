(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Reason
open Type_hint

let metadata =
  {
    Context.checked = true;
    include_suppressions = false;
    jsx = Options.Jsx_react;
    munge_underscores = false;
    strict = false;
    strict_local = false;
    (* Provide some useful error traces for debugging when some tests fail. *)
    verbose =
      Some Verbose.{ indent = 2; depth = 10; enabled_during_flowlib = false; focused_files = None };
    any_propagation = true;
    automatic_require_default = false;
    babel_loose_array_spread = false;
    cycle_errors = false;
    enable_const_params = false;
    enable_contextual_typing = true;
    enable_enums = true;
    enable_relay_integration = false;
    enforce_local_inference_annotations = false;
    enforce_strict_call_arity = true;
    enforce_this_annotations = false;
    env_mode = Options.ClassicEnv [];
    env_mode_constrain_write_dirs = [];
    exact_by_default = true;
    exact_empty_objects = false;
    experimental_infer_indexers = false;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    haste_module_ref_prefix = None;
    ignore_non_literal_requires = false;
    local_inference_annotation_dirs = [];
    max_literal_length = 100;
    max_trace_depth = 0;
    max_workers = 0;
    missing_module_generators = [];
    react_runtime = Options.ReactRuntimeClassic;
    react_server_component_exts = SSet.empty;
    recursion_limit = 10000;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root = Path.dummy_path;
    run_post_inference_implicit_instantiation = false;
    statement_reorder_checking = Options.Lexical;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
    type_asserts = false;
  }

let dummy_filename = File_key.SourceFile ""

let dummy_reason = locationless_reason (RCustom "dummy_reason")

(**************************)
(* Parse and convert type *)
(**************************)

module TypeParser : sig
  val parse : Context.t -> string -> Type.t
end = struct
  let parse_type content =
    let parse_options =
      Some
        {
          Parser_env.enums = true;
          esproposal_decorators = true;
          esproposal_export_star_as = true;
          types = true;
          use_strict = false;
        }
    in
    (* the parser expects a colon *)
    let content = ": " ^ content in
    let (t, errs) = Parser_flow.parse_annot ~parse_options (Some dummy_filename) content in
    assert (List.length errs = 0);
    Ast_loc_utils.loc_to_aloc_mapper#type_annotation t

  module New_env : Env_sig.S = New_env.New_env

  module rec Statement_ : (Statement_sig.S with module Env := New_env) =
    Statement.Make (New_env) (Destructuring_) (Func_stmt_config_) (Statement_)

  and Destructuring_ : Destructuring_sig.S = Destructuring.Make (New_env) (Statement_)

  and Func_stmt_config_ :
    (Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types) =
    Func_stmt_config.Make (New_env) (Destructuring_) (Statement_)

  module Annot = Type_annotation.Make (New_env) (Statement_.Abnormal) (Statement_)

  let parse cx content =
    let (_, t_ast) = parse_type content in
    let ((_, t), _) = Annot.convert cx Subst_name.Map.empty t_ast in
    t
end

let mk_cx () =
  let master_cx = Context.empty_master_cx () in
  let () =
    let reason =
      let loc = ALoc.none in
      let desc = Reason.RCustom "Explicit any used in type_hint tests" in
      Reason.mk_reason desc loc
    in
    (* Add builtins that will be used by tests. TODO: load real lib files. *)
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "console")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Array")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Object")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Generator")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "Promise")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "promise")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "$await")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "$Iterable")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "$AsyncIterable")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "React$Node")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "React$Key")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "React$Ref")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)));
    Builtins.set_builtin
      ~flow_t:(fun _ -> ())
      master_cx.Context.builtins
      (Reason.OrdinaryName "React$Element")
      (Type.AnyT (reason, Type.AnyError (Some Type.UnresolvedName)))
  in
  let aloc_table = lazy (ALoc.empty_table dummy_filename) in
  let ccx = Context.(make_ccx master_cx) in
  Context.make ccx metadata dummy_filename aloc_table Context.Checking

let mk_hint cx base ops =
  let base_t = TypeParser.parse cx base in
  ops
  |> Nel.of_list
  |> Base.Option.value_map ~default:(Hint_t base_t) ~f:(fun l -> Hint_Decomp (l, base_t))

let mk_eval_hint_test ~expected base ops ctxt =
  let cx = mk_cx () in
  let actual =
    mk_hint cx base ops
    |> Type_hint.evaluate_hint cx
    |> Base.Option.value_map ~default:"None" ~f:(Ty_normalizer.debug_string_of_t cx)
  in
  assert_equal ~ctxt ~printer:Base.Fn.id expected actual

let eval_hint_tests =
  [
    "hint_t_num" >:: mk_eval_hint_test ~expected:"number" "number" [];
    "hint_t_array" >:: mk_eval_hint_test ~expected:"Array<number>" "Array<number>" [];
  ]

let tests = "type_hint" >::: ["evaluate_hint" >::: eval_hint_tests]
