(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Reason
open Hint
open Loc_collections

let metadata =
  {
    Context.checked = true;
    include_suppressions = false;
    jsx = Options.Jsx_react;
    munge_underscores = false;
    strict = false;
    strict_local = false;
    available_platforms = None;
    has_explicit_supports_platform = false;
    (* Provide some useful error traces for debugging when some tests fail. *)
    verbose =
      Some Verbose.{ indent = 2; depth = 10; enabled_during_flowlib = false; focused_files = None };
    slow_to_check_logging = Slow_to_check_logging.default;
    any_propagation = true;
    automatic_require_default = false;
    babel_loose_array_spread = false;
    casting_syntax = Options.CastingSyntax.Colon;
    component_syntax = true;
    hooklike_functions_includes = [];
    hooklike_functions = true;
    react_rules = [];
    react_rules_always = false;
    enable_as_const = false;
    enable_const_params = false;
    enable_enums = true;
    enable_relay_integration = false;
    exact_by_default = true;
    facebook_fbs = None;
    facebook_fbt = None;
    facebook_module_interop = false;
    file_options = Files.default_options;
    ignore_non_literal_requires = false;
    max_literal_length = 100;
    max_trace_depth = 0;
    max_workers = 0;
    missing_module_generators = [];
    namespaces = false;
    react_runtime = Options.ReactRuntimeClassic;
    recursion_limit = 10000;
    relay_integration_esmodules = false;
    relay_integration_excludes = [];
    relay_integration_module_prefix = None;
    relay_integration_module_prefix_includes = [];
    root = File_path.dummy_path;
    strict_es6_import_export = false;
    strict_es6_import_export_excludes = [];
    strip_root = true;
    suppress_types = SSet.empty;
    ts_syntax = true;
    use_mixed_in_catch_variables = false;
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
        {
          Parser_env.components = true;
          enums = true;
          esproposal_decorators = true;
          types = true;
          use_strict = false;
          module_ref_prefix = None;
          module_ref_prefix_LEGACY_INTEROP = None;
        }
    in
    (* the parser expects a colon *)
    let content = ": " ^ content in
    let (t, errs) = Parser_flow.parse_annot ~parse_options (Some dummy_filename) content in
    assert (List.length errs = 0);
    Ast_loc_utils.loc_to_aloc_mapper#type_annotation t

  module Statement = Fix_statement.Statement_
  module Annot = Type_annotation.Make (Type_annotation_cons_gen.FlowJS) (Statement)
  module NameResolver = Name_resolver.Make (Context) (Flow_js_utils)

  let parse cx content =
    let (program_loc, t_ast) = parse_type content in
    (* The object type converter will peek the scope, so we need to have a non-empty scope list. *)
    let (_, info) =
      NameResolver.program_with_scope
        cx
        ( program_loc,
          {
            Flow_ast.Program.statements =
              [
                (* We construct a fake program `type _ = annot` with locations of annot will line
                   up with location of `annot` when it's parsed without `type _ = `. This will
                   allow name_resolver to correctly find the name-def relation to decide what's
                   globals and what's not. *)
                ( ALoc.none,
                  Flow_ast.Statement.TypeAlias
                    {
                      Flow_ast.Statement.TypeAlias.id =
                        (ALoc.none, { Flow_ast.Identifier.name = "_"; comments = None });
                      tparams = None;
                      right = t_ast;
                      comments = None;
                    }
                );
              ];
            interpreter = None;
            comments = None;
            all_comments = [];
          }
        )
    in
    let env =
      Loc_env.with_info
        Name_def.Global
        Loc_collections.ALocMap.empty
        info
        ALocMap.empty
        Env_api.EnvMap.empty
    in
    Context.set_environment cx env;
    Type_env.init_env cx Name_def.Global;
    let ((_, t), _) = Annot.convert cx Subst_name.Map.empty t_ast in
    t
end

module TypeLoader : sig
  val get_master_cx : unit -> Context.master_context

  val get_type_of_last_expression : Context.t -> string -> Type.t
end = struct
  let parse_content file content =
    let parse_options =
      Some
        {
          Parser_env.components = true;
          enums = true;
          (*
           * Always parse ES proposal syntax. The user-facing config option to
           * ignore/warn/enable them is handled during inference so that a clean error
           * can be surfaced (rather than a more cryptic parse error).
           *)
          esproposal_decorators = true;
          types = true;
          use_strict = false;
          module_ref_prefix = None;
          module_ref_prefix_LEGACY_INTEROP = None;
        }
    in
    let (ast, _) = Parser_flow.program_file ~fail:false ~parse_options content (Some file) in
    ast

  (* No verbose mode during libdef init. *)
  let metadata = { metadata with Context.verbose = None }

  let init_master_cx () =
    let asts =
      Flowlib.contents_list ~no_flowlib:false
      |> List.rev_map (fun (filename, lib_content) ->
             let lib_file = File_key.LibFile filename in
             let ast = parse_content lib_file lib_content in
             ast
         )
    in
    let sig_opts =
      let open Type_sig_options in
      {
        munge = false;
        facebook_keyMirror = false;
        enable_relay_integration = false;
        relay_integration_module_prefix = None;
        suppress_types = SSet.empty;
        facebook_fbt = None;
        max_literal_len = 100;
        exact_by_default = true;
        enable_enums = true;
        enable_component_syntax = true;
        enable_ts_syntax = true;
        hooklike_functions = true;
        casting_syntax = Options.CastingSyntax.Both;
        for_builtins = true;
        locs_to_dirtify = [];
      }
    in
    Merge_js.merge_lib_files ~sig_opts asts |> snd

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
    let ast = parse_content dummy_filename content in
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
      t
    | _ -> failwith "Must have a last statement that's an expression"
end

let fun_t ~params ~return_t =
  let open Type in
  DefT
    ( dummy_reason,
      FunT
        ( Unsoundness.dummy_static_any dummy_reason,
          {
            this_t = (Unsoundness.unresolved_any dummy_reason, This_Function);
            params;
            rest_param = None;
            return_t;
            predicate = None;
            def_reason = dummy_reason;
            hook = AnyHook;
          }
        )
    )

let mk_cx ~verbose () =
  let aloc_table = lazy (ALoc.empty_table dummy_filename) in
  let resolve_require mref = Error (Reason.internal_module_name mref) in
  let ccx = Context.make_ccx () in
  let metadata =
    if verbose then
      metadata
    else
      { metadata with Context.verbose = None }
  in
  Context.make
    ccx
    metadata
    dummy_filename
    aloc_table
    resolve_require
    (Merge_js.mk_builtins metadata (TypeLoader.get_master_cx ()))

let mk_hint base_t ops =
  ops
  |> List.mapi (fun i op -> (i, op))
  |> Nel.of_list
  |> Base.Option.value_map
       ~default:(Hint_t (base_t, ExpectedTypeHint))
       ~f:(fun l -> Hint_Decomp (l, base_t, ExpectedTypeHint))

let string_of_hint_eval_result cx = function
  | Type.HintAvailable (t, _) -> Ty_normalizer_flow.debug_string_of_t cx t
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
  let class_bindings = { class_binding_id = ALoc.id_none } in
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
        InstanceT
          {
            static = ObjProtoT dummy_reason;
            super = ObjProtoT dummy_reason;
            implements = [];
            inst =
              {
                class_id = ALoc.id_none;
                class_name = None;
                type_args = [];
                own_props = Context.generate_property_map cx NameUtils.Map.empty;
                proto_props = Context.generate_property_map cx NameUtils.Map.empty;
                inst_call_t = None;
                initialized_fields = SSet.empty;
                initialized_static_fields = SSet.empty;
                inst_kind = ClassKind;
                inst_dict = None;
                class_private_fields = mk_propertries private_field;
                class_private_static_fields = mk_propertries private_static_field;
                class_private_methods = mk_propertries private_method;
                class_private_static_methods = mk_propertries private_static_method;
              };
          }
      )
  in
  let base =
    if static then
      DefT (dummy_reason, ClassT base)
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
  let base_t = TypeLoader.get_type_of_last_expression cx type_setup_code in
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
    >:: mk_eval_hint_test
          ~expected:"string"
          "(string, number) => number"
          [Decomp_FuncParam ([None], 0, None)];
    "fun_decomp_union"
    >:: mk_eval_hint_test
          ~expected:"string"
          "(string) => number | (string) => number"
          [Decomp_FuncParam ([None], 0, None)];
    "fun_decomp_simple_on_second_argument_of_hint"
    >:: mk_eval_hint_test
          ~expected:"number"
          "(string, number) => number"
          [Decomp_FuncParam ([None; None], 1, None)];
    "fun_decomp_on_nonexistent_argument_of_hint"
    >:: mk_eval_hint_test ~expected:"void" "() => number" [Decomp_FuncParam ([None], 0, None)];
    "fun_decomp_on_rest_arguments_of_hint"
    >:: mk_eval_hint_test
          ~expected:"number"
          "(...Array<number>) => number"
          [Decomp_FuncParam ([None], 0, None)];
    "fun_decomp_rest_arguments_matching_number_of_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<number>"
          "(string, number, ...Array<number>) => number"
          [Decomp_FuncRest ([None; None], None)];
    "fun_decomp_rest_arguments_with_additional_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<string>"
          "(string, number, ...Array<string>) => number"
          [Decomp_FuncRest ([None; None; None], None)];
    "fun_decomp_rest_arguments_overlap_with_normal_parameters"
    >:: mk_eval_hint_test
          ~expected:"Array<(number | string)>"
          "(string, number, ...Array<string>) => number"
          [Decomp_FuncRest ([None], None)];
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
          [Decomp_FuncParam ([None], 0, None); Decomp_FuncParam ([None], 0, None)];
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
    >:: mk_eval_hint_test
          ~expected:"(mixed) => void"
          "<T>(T => void) => void"
          [Decomp_FuncParam ([None], 0, None)];
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
    >:: mk_eval_hint_test ~expected:"{foo: number}" "{+foo: number}" [Decomp_ObjSpread];
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
                {
                  key_loc = Some ALoc.none;
                  type_ = fun_t ~params:[] ~return_t:(VoidT.make dummy_reason);
                }
            );
    "instance_private_method_call_from_static_method"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:true
          ~private_static_method:
            Type.(
              Method
                {
                  key_loc = Some ALoc.none;
                  type_ = fun_t ~params:[] ~return_t:(VoidT.make dummy_reason);
                }
            );
    "instance_private_method_call_from_field"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:false
          ~private_field:
            Type.(
              Field
                {
                  preferred_def_locs = None;
                  key_loc = Some ALoc.none;
                  type_ = fun_t ~params:[] ~return_t:(VoidT.make dummy_reason);
                  polarity = Polarity.Neutral;
                }
            );
    "instance_private_method_call_from_static_field"
    >:: mk_private_method_eval_hint_test
          ~expected:"() => void"
          ~static:true
          ~private_static_field:
            Type.(
              Field
                {
                  preferred_def_locs = None;
                  key_loc = Some ALoc.none;
                  type_ = fun_t ~params:[] ~return_t:(VoidT.make dummy_reason);
                  polarity = Polarity.Neutral;
                }
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
          "React$AbstractComponent<{+bar: string, +foo: number}, mixed, React$Node>"
          [Decomp_JsxProps];
    "jsx_props_select"
    >:: mk_eval_hint_test
          ~expected:"number"
          "({bar: string, foo: number}) => number"
          [Decomp_ObjProp "foo"; Decomp_JsxProps];
    "jsx_props_spread"
    >:: mk_eval_hint_test
          ~expected:"{bar: string, foo: number}"
          "({bar: string, foo: number}) => number"
          [Decomp_ObjSpread; Decomp_JsxProps];
    "jsx_ref_function"
    >:: mk_eval_hint_test
          ~expected:"React$ElementRef<React$AbstractComponent<{...}, string, React$Node>> | null"
          "React$AbstractComponent<{...}, string, React$Node>"
          [Decomp_FuncParam ([None], 0, None); Decomp_JsxRef];
    "jsx_fragment_ref"
    >:: mk_eval_hint_test_with_type_setup
          ~expected:"void"
          "declare var Fragment: React$FragmentType; Fragment"
          [Decomp_ObjProp "children"; Decomp_JsxProps];
  ]

let tests = "type_hint" >::: ["evaluate_hint" >::: eval_hint_tests]
