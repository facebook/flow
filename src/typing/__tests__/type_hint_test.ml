(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Reason
open Hint_api

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
    conditional_type = false;
    cycle_errors = false;
    cycle_errors_includes = [];
    enable_const_params = false;
    enable_enums = true;
    enable_relay_integration = false;
    enforce_strict_call_arity = true;
    inference_mode = Options.LTI;
    inference_mode_lti_includes = [];
    exact_by_default = true;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    haste_module_ref_prefix = None;
    ignore_non_literal_requires = false;
    max_literal_length = 100;
    max_trace_depth = 0;
    max_workers = 0;
    missing_module_generators = [];
    array_literal_providers = false;
    array_literal_providers_includes = [];
    react_runtime = Options.ReactRuntimeClassic;
    react_server_component_exts = SSet.empty;
    recursion_limit = 10000;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root = Path.dummy_path;
    run_post_inference_implicit_instantiation = false;
    enable_post_inference_targ_widened_check = false;
    save_implicit_instantiation_results = false;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    trust_mode = Options.NoTrust;
  }

let dummy_filename = File_key.SourceFile ""

let dummy_loc = Loc.mk_loc ~source:dummy_filename (0, 0) (0, 0) |> ALoc.of_loc

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
        { Parser_env.enums = true; esproposal_decorators = true; types = true; use_strict = false }
    in
    (* the parser expects a colon *)
    let content = ": " ^ content in
    let (t, errs) = Parser_flow.parse_annot ~parse_options (Some dummy_filename) content in
    assert (List.length errs = 0);
    Ast_loc_utils.loc_to_aloc_mapper#type_annotation t

  module Statement = Fix_statement.Statement_
  module Annot = Type_annotation.Make (Type_annotation.FlowJS) (Statement)
  module NameResolver = Name_resolver.Make_of_flow (Context) (Flow_js_utils)

  let parse cx content =
    let (program_loc, t_ast) = parse_type content in
    (* The object type converter will peek the scope, so we need to have a non-empty scope list. *)
    let (_, info) =
      NameResolver.program_with_scope
        cx
        (program_loc, { Flow_ast.Program.statements = []; comments = None; all_comments = [] })
    in
    let env = Loc_env.with_info Name_def.Global Loc_collections.ALocMap.empty info in
    Context.set_environment cx env;
    Env.init_env cx Name_def.Global;
    let ((_, t), _) = Annot.convert cx Subst_name.Map.empty t_ast in
    t
end

module TypeLoader : sig
  val get_master_cx : unit -> Context.master_context

  val get_type_of_last_expression : Context.t -> string -> Context.t * Type.t
end = struct
  let parse_content file content =
    let parse_options =
      Some
        {
          Parser_env.enums = true;
          (*
           * Always parse ES proposal syntax. The user-facing config option to
           * ignore/warn/enable them is handled during inference so that a clean error
           * can be surfaced (rather than a more cryptic parse error).
           *)
          esproposal_decorators = true;
          types = true;
          use_strict = false;
        }
    in

    let (ast, _) = Parser_flow.program_file ~fail:false ~parse_options content (Some file) in
    let (fsig, _) = File_sig.With_Loc.program ~ast ~opts:File_sig.With_Loc.default_opts in
    (ast, fsig)

  (* No verbose mode during libdef init. *)
  let metadata = { metadata with Context.verbose = None }

  let load_lib_files ccx =
    (* iterate in reverse override order *)
    let (leader, _) =
      Flowlib.contents_list ~no_flowlib:false
      |> List.fold_left
           (fun (_, exclude_syms) (filename, lib_content) ->
             let lib_file = File_key.LibFile filename in
             let (ast, file_sig) = parse_content lib_file lib_content in
             (* Lib files use only concrete locations, so this is not used. *)
             let aloc_table = lazy (ALoc.empty_table lib_file) in
             let cx = Context.make ccx metadata lib_file aloc_table Context.Checking in
             let (syms, _) =
               Type_inference_js.infer_lib_file
                 cx
                 ast
                 ~exclude_syms
                 ~file_sig:(File_sig.abstractify_locs file_sig)
                 ~lint_severities:LintSettings.empty_severities
             in
             (* symbols loaded from this file are suppressed if found in later ones *)
             (Some cx, NameUtils.Set.union exclude_syms (NameUtils.Set.of_list syms)))
           (None, NameUtils.Set.empty)
    in
    leader

  let init_master_cx () =
    let ccx = Context.(make_ccx (empty_master_cx ())) in
    match load_lib_files ccx with
    | None -> Context.empty_master_cx ()
    | Some cx ->
      Merge_js.optimize_builtins cx;
      { Context.master_sig_cx = Context.sig_cx cx; builtins = Context.builtins cx }

  let master_cx_ref = ref None

  let get_master_cx () =
    match !master_cx_ref with
    | Some master_cx -> master_cx
    | None ->
      let master_cx = init_master_cx () in
      master_cx_ref := Some master_cx;
      master_cx

  let get_typed_ast cx content =
    let metadata = { metadata with Context.verbose = None } in
    let (ast, file_sig) = parse_content dummy_filename content in
    let file_sig = File_sig.abstractify_locs file_sig in
    (* connect requires *)
    Type_inference_js.add_require_tvars cx file_sig;
    let connect_requires mref =
      let module_name = Reason.internal_module_name mref in
      Nel.iter (fun loc ->
          let reason = Reason.(mk_reason (RCustom mref) loc) in
          let module_t = Flow_js_utils.lookup_builtin_strict cx module_name reason in
          let (_, require_id) = Context.find_require cx loc in
          Flow_js.resolve_id cx require_id module_t
      )
    in
    SMap.iter connect_requires File_sig.With_ALoc.(require_loc_map file_sig.module_sig);
    let ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
    let lint_severities =
      Merge_js.get_lint_severities metadata StrictModeSettings.empty LintSettings.empty_severities
    in
    Type_inference_js.infer_ast cx dummy_filename [] ast ~lint_severities

  let get_type_of_last_expression cx content =
    let (_, { Flow_ast.Program.statements; _ }) = get_typed_ast cx content in
    match Base.List.last statements with
    | Some
        ( _,
          Flow_ast.Statement.Expression
            { Flow_ast.Statement.Expression.expression = ((_, t), _); _ }
        ) ->
      let reducer =
        new Context_optimizer.context_optimizer ~no_lowers:(fun _ r -> Type.Unsoundness.merged_any r)
      in
      let file = Context.file cx in
      let metadata = Context.metadata cx in
      let aloc_table = Utils_js.FilenameMap.find file (Context.aloc_tables cx) in
      let ccx = Context.make_ccx (get_master_cx ()) in
      let t = reducer#type_ cx Polarity.Neutral t in
      Context.merge_into
        ccx
        {
          Type.TypeContext.graph = reducer#get_reduced_graph;
          trust_graph = reducer#get_reduced_trust_graph;
          property_maps = reducer#get_reduced_property_maps;
          call_props = reducer#get_reduced_call_props;
          export_maps = reducer#get_reduced_export_maps;
          evaluated = reducer#get_reduced_evaluated;
        };
      let cx = Context.make ccx metadata file aloc_table Context.PostInference in
      (cx, t)
    | _ -> failwith "Must have a last statement that's an expression"
end

let fun_t ~params ~return_t =
  let open Type in
  DefT
    ( dummy_reason,
      bogus_trust (),
      FunT
        ( Unsoundness.dummy_static_any dummy_reason,
          {
            this_t = (Unsoundness.unresolved_any dummy_reason, This_Function);
            params;
            rest_param = None;
            return_t;
            is_predicate = false;
            def_reason = dummy_reason;
          }
        )
    )

let mk_cx ~verbose () =
  let master_cx = TypeLoader.get_master_cx () in
  let aloc_table = lazy (ALoc.empty_table dummy_filename) in
  let ccx = Context.(make_ccx master_cx) in
  let metadata =
    if verbose then
      metadata
    else
      { metadata with Context.verbose = None }
  in
  Context.make ccx metadata dummy_filename aloc_table Context.Checking

let mk_hint base_t ops =
  ops
  |> List.mapi (fun i op -> (i, op))
  |> Nel.of_list
  |> Base.Option.value_map ~default:(Hint_t base_t) ~f:(fun l -> Hint_Decomp (l, base_t))

let string_of_hint_eval_result cx = function
  | Type.HintAvailable t -> Ty_normalizer.debug_string_of_t cx t
  | Type.NoHint -> "NoHint"
  | Type.EncounteredPlaceholder -> "EncounteredPlaceholder"
  | Type.DecompositionError -> "DecompositionError"

let mk_eval_hint_test ~expected base ops ctxt =
  let cx = mk_cx ~verbose:false () in
  let actual =
    mk_hint (TypeParser.parse cx base) ops
    |> Type_hint.evaluate_hint cx dummy_reason
    |> string_of_hint_eval_result cx
  in
  (* DEBUGGING TIP: set [~verbose:true] above to print traces *)
  assert_equal ~ctxt ~printer:Base.Fn.id expected actual

let mk_private_method_eval_hint_test
    ~expected
    ~static
    ?private_field
    ?private_static_field
    ?private_method
    ?private_static_method
    ctxt =
  let open Type in
  let cx = mk_cx ~verbose:false () in
  let env = Context.environment cx in
  let mk_propertries = function
    | None -> NameUtils.Map.empty |> Context.generate_property_map cx
    | Some property ->
      NameUtils.Map.singleton (OrdinaryName "bar") property |> Context.generate_property_map cx
  in
  let class_bindings =
    {
      class_binding_id = ALoc.id_none;
      class_private_fields = mk_propertries private_field;
      class_private_static_fields = mk_propertries private_static_field;
      class_private_methods = mk_propertries private_method;
      class_private_static_methods = mk_propertries private_static_method;
    }
  in
  let class_stack_loc = ALoc.none in
  Context.set_environment
    cx
    {
      env with
      Loc_env.class_bindings = Loc_collections.ALocMap.singleton class_stack_loc class_bindings;
    };
  let base =
    DefT
      ( dummy_reason,
        bogus_trust (),
        InstanceT
          ( ObjProtoT dummy_reason,
            ObjProtoT dummy_reason,
            [],
            {
              class_id = ALoc.id_none;
              type_args = [];
              own_props = Context.generate_property_map cx NameUtils.Map.empty;
              proto_props = Context.generate_property_map cx NameUtils.Map.empty;
              inst_call_t = None;
              initialized_fields = SSet.empty;
              initialized_static_fields = SSet.empty;
              has_unknown_react_mixins = false;
              inst_kind = ClassKind;
            }
          )
      )
  in
  let base =
    if static then
      DefT (dummy_reason, bogus_trust (), ClassT base)
    else
      base
  in
  let actual =
    mk_hint base [Decomp_MethodPrivateName ("bar", [class_stack_loc])]
    |> Type_hint.evaluate_hint cx dummy_reason
    |> string_of_hint_eval_result cx
  in
  assert_equal ~ctxt ~printer:Base.Fn.id expected actual

let mk_eval_hint_test_with_type_setup ~expected type_setup_code ops ctxt =
  let cx = mk_cx ~verbose:false () in
  let (cx, base_t) = TypeLoader.get_type_of_last_expression cx type_setup_code in
  let actual =
    mk_hint base_t ops |> Type_hint.evaluate_hint cx dummy_reason |> string_of_hint_eval_result cx
  in
  (* DEBUGGING TIP: set [~verbose:true] above to print traces *)
  assert_equal ~ctxt ~printer:Base.Fn.id expected actual

let eval_hint_tests =
  [
    "hint_t_num" >:: mk_eval_hint_test ~expected:"number" "number" [];
    "hint_t_array" >:: mk_eval_hint_test ~expected:"Array<number>" "Array<number>" [];
    "array_element_decomp_general"
    >:: mk_eval_hint_test ~expected:"number" "Array<number>" [Decomp_ArrElement None];
    "array_element_decomp_specific"
    >:: mk_eval_hint_test ~expected:"string" "[number, string]" [Decomp_ArrElement (Some 1)];
    "array_element_decomp_specific"
    >:: mk_eval_hint_test ~expected:"number | string" "[number, string]" [Decomp_ArrElement None];
    "array_element_decomp_specific_nonexistent"
    >:: mk_eval_hint_test
          ~expected:"DecompositionError"
          "[number, string]"
          [Decomp_ArrElement (Some 2)];
    "array_spread_decomp_with_general"
    >:: mk_eval_hint_test ~expected:"Array<number>" "Array<number>" [Decomp_ArrSpread 0];
    "array_spread_decomp_with_tuple_full"
    >:: mk_eval_hint_test ~expected:"[number, string]" "[number, string]" [Decomp_ArrSpread 0];
    "array_spread_decomp_with_tuple_single"
    >:: mk_eval_hint_test ~expected:"[string]" "[number, string]" [Decomp_ArrSpread 1];
    "array_spread_decomp_with_tuple_part"
    >:: mk_eval_hint_test
          ~expected:"[string, number]"
          "[number, string, number]"
          [Decomp_ArrSpread 1];
    "await_decomp"
    >:: mk_eval_hint_test ~expected:"string | Promise<string>" "string" [Decomp_Await];
    "fun_decomp_simple_return"
    >:: mk_eval_hint_test ~expected:"number" "(string, number) => number" [Decomp_FuncReturn];
    "fun_decomp_multi_spec_return"
    >:: mk_eval_hint_test ~expected:"number" "?(string, number) => number" [Decomp_FuncReturn];
    "fun_decomp_simple_on_first_argument_of_hint"
    >:: mk_eval_hint_test ~expected:"string" "(string, number) => number" [Decomp_FuncParam 0];
    "fun_decomp_union"
    >:: mk_eval_hint_test
          ~expected:"string"
          "(string) => number | (string) => number"
          [Decomp_FuncParam 0];
    "fun_decomp_simple_on_second_argument_of_hint"
    >:: mk_eval_hint_test ~expected:"number" "(string, number) => number" [Decomp_FuncParam 1];
    "fun_decomp_on_nonexistent_argument_of_hint"
    >:: mk_eval_hint_test ~expected:"void" "() => number" [Decomp_FuncParam 0];
    "fun_decomp_on_rest_arguments_of_hint"
    >:: mk_eval_hint_test ~expected:"number" "(...Array<number>) => number" [Decomp_FuncParam 0];
    "fun_decomp_rest_arguments_matching_number_of_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<number>"
          "(string, number, ...Array<number>) => number"
          [Decomp_FuncRest 2];
    "fun_decomp_rest_arguments_with_additional_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<string>"
          "(string, number, ...Array<string>) => number"
          [Decomp_FuncRest 3];
    "fun_decomp_rest_arguments_overlap_with_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<(number | string)>"
          "(string, number, ...Array<string>) => number"
          [Decomp_FuncRest 1];
    (*
      When we try to extract the hint for the annotated parameter in
      ```
      declare function f<T>(T): void;
      f((a) => ...);
      ```
      It should fail because we cannot destruct a type parameter without any bounds.
    *)
    "fun_decomp_cannot_decomp_no_bound_type_parameter"
    >:: mk_eval_hint_test
          ~expected:"DecompositionError"
          "<T>(T) => void"
          [Decomp_FuncParam 0; Decomp_FuncParam 0];
    (*
      When we try to extract the hint for the lambda in
      ```
      declare function f<T>(T => void) => void;
      f((a) => {});
      ```
      In Pierce's algorithm, this is the case when the lambda must be fully annotated. However, we
      can still choose to assign the lambda parameter a type by using the most conservative type we
      can: mixed. If the users want something more precise, they will have to annotate.
    *)
    "fun_decomp_unsolved_type_parameter"
    >:: mk_eval_hint_test ~expected:"(mixed) => void" "<T>(T => void) => void" [Decomp_FuncParam 0];
    "obj_prop_from_record_neutral_polarity"
    >:: mk_eval_hint_test ~expected:"number" "{foo: number}" [Decomp_ObjProp "foo"];
    "obj_prop_from_record_positive_polarity"
    >:: mk_eval_hint_test ~expected:"number" "{+foo: number}" [Decomp_ObjProp "foo"];
    "obj_prop_from_record_positive_polarity_from_readonly"
    >:: mk_eval_hint_test ~expected:"number" "$ReadOnly<{foo: number}>" [Decomp_ObjProp "foo"];
    "obj_prop_from_record_negative_polarity"
    >:: mk_eval_hint_test ~expected:"DecompositionError" "{-foo: number}" [Decomp_ObjProp "foo"];
    "obj_prop_from_dict_neutral_polarity"
    >:: mk_eval_hint_test ~expected:"number" "{[string]: number}" [Decomp_ObjProp "foo"];
    "obj_prop_from_dict_positive_polarity"
    >:: mk_eval_hint_test ~expected:"number" "{+[string]: number}" [Decomp_ObjProp "foo"];
    "obj_prop_from_dict_negative_polarity"
    >:: mk_eval_hint_test
          ~expected:"DecompositionError"
          "{-[string]: number}"
          [Decomp_ObjProp "foo"];
    "obj_prop_union"
    >:: mk_eval_hint_test
          ~expected:"number | string"
          "{foo: number} | {[string]: string}"
          [Decomp_ObjProp "foo"];
    "obj_prop_union_some_without_prop"
    >:: mk_eval_hint_test
          ~expected:"number | string | void"
          "{foo: number} | {[string]: string} | {bar: string}"
          [Decomp_ObjProp "foo"];
    "obj_prop_union_some_failed"
    >:: mk_eval_hint_test
          ~expected:"number | string"
          "{foo: number} | {[string]: string} | number"
          [Decomp_ObjProp "foo"];
    "obj_prop_optional"
    >:: mk_eval_hint_test ~expected:"number" "?{foo: number}" [Decomp_ObjProp "foo"];
    "obj_prop_intersection"
    >:: mk_eval_hint_test
          ~expected:"number"
          "{bar: string} & {foo: number} & {[string]: string}"
          [Decomp_ObjProp "foo"];
    "obj_prop_from_prototype"
    >:: mk_eval_hint_test ~expected:"number" "string" [Decomp_ObjProp "length"];
    "obj_rest_from_record_neutral_polarity"
    >:: mk_eval_hint_test ~expected:"{foo: number}" "{foo: number}" [Decomp_ObjSpread];
    "obj_rest_from_record_positive_polarity"
    >:: mk_eval_hint_test ~expected:"{+foo: number}" "{+foo: number}" [Decomp_ObjSpread];
    "obj_rest_from_record_negative_polarity"
    >:: mk_eval_hint_test ~expected:"{-foo: number}" "{-foo: number}" [Decomp_ObjSpread];
    "obj_rest_from_dict_neutral_polarity"
    >:: mk_eval_hint_test ~expected:"{[string]: number}" "{[string]: number}" [Decomp_ObjSpread];
    "obj_rest_from_dict_positive_polarity"
    >:: mk_eval_hint_test ~expected:"{+[string]: number}" "{+[string]: number}" [Decomp_ObjSpread];
    "obj_rest_from_dict_negative_polarity"
    >:: mk_eval_hint_test ~expected:"{-[string]: number}" "{-[string]: number}" [Decomp_ObjSpread];
    "method_name_from_instance"
    >:: mk_eval_hint_test ~expected:"() => string" "string" [Decomp_MethodName "toString"];
    "method_name_from_object"
    >:: mk_eval_hint_test ~expected:"() => number" "{foo: () => number}" [Decomp_MethodName "foo"];
    "method_elem_from_dict"
    >:: mk_eval_hint_test ~expected:"() => number" "{[string]: () => number}" [Decomp_MethodElem];
    "instance_private_method_call_from_method"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:false
          ~private_method:
            Type.(
              Method
                ( Some ALoc.none,
                  fun_t ~params:[] ~return_t:(VoidT.make dummy_reason (bogus_trust ()))
                )
            );
    "instance_private_method_call_from_static_method"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:true
          ~private_static_method:
            Type.(
              Method
                ( Some ALoc.none,
                  fun_t ~params:[] ~return_t:(VoidT.make dummy_reason (bogus_trust ()))
                )
            );
    "instance_private_method_call_from_field"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:false
          ~private_field:
            Type.(
              Field
                ( Some ALoc.none,
                  fun_t ~params:[] ~return_t:(VoidT.make dummy_reason (bogus_trust ())),
                  Polarity.Neutral
                )
            );
    "instance_private_method_call_from_static_field"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:true
          ~private_static_field:
            Type.(
              Field
                ( Some ALoc.none,
                  fun_t ~params:[] ~return_t:(VoidT.make dummy_reason (bogus_trust ())),
                  Polarity.Neutral
                )
            );
    "call_new_from_class"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"(bar: number, baz: boolean) => Foo"
          "class Foo { constructor(bar: number, baz: boolean) {} }; Foo"
          [Decomp_CallNew];
    "call_new_from_class_default_ctor"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"() => Foo"
          "class Foo { }; Foo"
          [Decomp_CallNew];
    "call_new_from_class_polymorphic"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"<A, T>(a: A, t: T) => Foo<A>"
          "declare class Foo<A> { constructor<T>(a: A, t: T): void }; Foo"
          [Decomp_CallNew];
    "call_new_from_class_overload"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"(<A>(a: A, t: number) => Foo<A>) & (<A>(a: A) => Foo<A>)"
          "declare class Foo<A> { constructor(a: A, t: number): void; constructor(a: A): void; }; Foo"
          [Decomp_CallNew];
    "call_new_from_class_polymorphic_overload"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"(<A, T>(a: A, t: T) => Foo<A>) & (<A, T>(a: A) => Foo<A>)"
          "declare class Foo<A> { constructor<T>(a: A, t: T): void; constructor<T>(a: A): void; }; Foo"
          [Decomp_CallNew];
    "call_super"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"(bar: number, baz: boolean) => Foo"
          "class Foo { constructor(bar: number, baz: boolean) {} }; new Foo()"
          [Decomp_CallSuper];
    "jsx_props_of_class_component"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"{+bar: string, +foo: number}"
          "class MyComponent extends React$Component<{bar: string, foo: number}> {}; MyComponent"
          [Decomp_JsxProps];
    "jsx_props_of_function_component"
    >:: mk_eval_hint_test
          ~expected:"{+bar: string, +foo: number}"
          "({bar: string, foo: number}) => number"
          [Decomp_JsxProps];
    "jsx_props_of_abstract_component"
    >:: mk_eval_hint_test
          ~expected:"{+bar: string, +foo: number}"
          "React$AbstractComponent<{+bar: string, +foo: number}, mixed>"
          [Decomp_JsxProps];
    "jsx_props_select"
    >:: mk_eval_hint_test
          ~expected:"number"
          "({bar: string, foo: number}) => number"
          [Decomp_ObjProp "foo"; Decomp_JsxProps];
    "jsx_props_spread"
    >:: mk_eval_hint_test
          ~expected:"{+bar: string, +foo: number}"
          "({bar: string, foo: number}) => number"
          [Decomp_ObjSpread; Decomp_JsxProps];
    "jsx_ref_function"
    >:: mk_eval_hint_test
          ~expected:"React$ElementRef<React$AbstractComponent<{...}, string>> | null"
          "React$AbstractComponent<{...}, string>"
          [Decomp_FuncParam 0; Decomp_JsxRef];
    "jsx_fragment_ref"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"void | React$Node"
          "declare var Fragment: React$FragmentType; Fragment"
          [Decomp_ObjProp "children"; Decomp_JsxProps];
  ]

let tests = "type_hint" >::: ["evaluate_hint" >::: eval_hint_tests]
