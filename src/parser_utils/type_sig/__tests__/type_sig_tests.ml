(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type_sig
open Type_sig_collections
module Parse = Type_sig_parse
module Pack = Type_sig_pack

let indent_len str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && str.[!i] = ' ' do
    incr i
  done;
  !i

let dedent_trim str =
  let lines = String.split_on_char '\n' str in
  let lines = List.filter (fun line -> String.trim line <> "") lines in
  let min_indent = List.fold_left (fun acc line -> min acc (indent_len line)) max_int lines in
  let lines =
    List.map
      (fun line ->
        let len = String.length line in
        String.sub line min_indent (len - min_indent))
      lines
  in
  String.concat "\n" lines

let mk_pp_loc locs =
  let open Format in
  let open Loc in
  fun fmt loc ->
    let loc = Locs.get locs loc in
    if loc.start = loc._end then
      fprintf fmt "[%d:%d]" loc.start.line loc.start.column
    else if loc.start.line = loc._end.line then
      fprintf fmt "[%d:%d-%d]" loc.start.line loc.start.column loc._end.column
    else
      fprintf fmt "[%d:%d-%d:%d]" loc.start.line loc.start.column loc._end.line loc._end.column

let pp_module_kind pp_loc = Pack.pp_module_kind pp_loc

let pp_pattern pp_loc = Pack.pp_pattern pp_loc

let pp_def pp_loc = Pack.pp_packed_def pp_loc

let pp_packed pp_loc = Pack.pp_packed pp_loc

let pp_remote_ref pp_loc = Pack.pp_remote_ref pp_loc

let pp_module_refs fmt module_refs =
  let open Format in
  if Module_refs.length module_refs > 0 then fprintf fmt "@.Module refs:@.";
  Module_refs.iteri
    (fun i mref ->
      fprintf fmt "%d. %s" (i :> int) mref;
      pp_print_newline fmt ())
    module_refs

let pp_local_defs pp_loc fmt local_defs dirty_indices =
  let open Format in
  if Local_defs.length local_defs > 0 then fprintf fmt "@.Local defs:@.";
  Local_defs.iteri
    (fun i def ->
      let i = (i :> int) in
      let dirty_prefix =
        if Array.mem i dirty_indices then
          "(dirty) "
        else
          ""
      in
      fprintf fmt "%s%d. " dirty_prefix i;
      pp_def pp_loc fmt def;
      pp_print_newline fmt ())
    local_defs

let pp_remote_refs pp_loc fmt remote_refs =
  let open Format in
  if Remote_refs.length remote_refs > 0 then fprintf fmt "@.Remote refs:@.";
  Remote_refs.iteri
    (fun i ref ->
      fprintf fmt "%d. " (i :> int);
      pp_remote_ref pp_loc fmt ref;
      pp_print_newline fmt ())
    remote_refs

let pp_pattern_defs pp_loc fmt pattern_defs dirty_indices =
  let open Format in
  if Pattern_defs.length pattern_defs > 0 then fprintf fmt "@.Pattern defs:@.";
  Pattern_defs.iteri
    (fun i def ->
      let i = (i :> int) in
      let dirty_prefix =
        if Array.mem i dirty_indices then
          "(dirty) "
        else
          ""
      in
      fprintf fmt "%s%d. " dirty_prefix i;
      pp_packed pp_loc fmt def;
      pp_print_newline fmt ())
    pattern_defs

let pp_patterns pp_loc fmt patterns =
  let open Format in
  if Patterns.length patterns > 0 then fprintf fmt "@.Patterns:@.";
  Patterns.iteri
    (fun i def ->
      fprintf fmt "%d. " (i :> int);
      pp_pattern pp_loc fmt def;
      pp_print_newline fmt ())
    patterns

let pp_errors pp_loc fmt errs =
  let open Format in
  if errs <> [] then (
    fprintf fmt "@.Errors:@.";
    pp_print_list ~pp_sep:pp_print_newline (pp_error pp_loc) fmt errs;
    pp_print_newline fmt ()
  )

let pp_sig
    fmt
    ( errs,
      locs,
      {
        Packed_type_sig.Module.module_kind;
        module_refs;
        local_defs;
        remote_refs;
        pattern_defs;
        patterns;
        dirty_local_defs;
        dirty_pattern_defs;
      }
    ) =
  let open Format in
  let pp_loc = mk_pp_loc locs in
  pp_module_kind pp_loc fmt module_kind;
  pp_print_newline fmt ();
  pp_module_refs fmt module_refs;
  pp_local_defs pp_loc fmt local_defs dirty_local_defs;
  pp_remote_refs pp_loc fmt remote_refs;
  pp_pattern_defs pp_loc fmt pattern_defs dirty_pattern_defs;
  pp_patterns pp_loc fmt patterns;
  pp_errors pp_loc fmt errs

let pp_builtin_module pp_loc fmt { Packed_type_sig.Builtins.loc; module_kind } =
  let open Format in
  fprintf fmt "%a " pp_loc loc;
  pp_module_kind pp_loc fmt module_kind

let pp_builtins
    fmt
    ( errs,
      locs,
      {
        Packed_type_sig.Builtins.module_refs;
        local_defs;
        remote_refs;
        pattern_defs;
        patterns;
        global_values;
        global_types;
        global_modules;
      }
    ) =
  let open Format in
  let pp_loc = mk_pp_loc locs in
  pp_module_refs fmt module_refs;
  pp_local_defs pp_loc fmt local_defs [||];
  pp_remote_refs pp_loc fmt remote_refs;
  pp_pattern_defs pp_loc fmt pattern_defs [||];
  pp_patterns pp_loc fmt patterns;
  SMap.iter (fun name _ -> fprintf fmt "@.Builtin global value %s@?" name) global_values;
  SMap.iter (fun name _ -> fprintf fmt "@.Builtin global type %s@?" name) global_types;
  SMap.iter
    (fun name m ->
      fprintf fmt "@.Builtin module %s:@." name;
      pp_builtin_module pp_loc fmt m)
    global_modules;
  pp_errors pp_loc fmt errs

let make_test_formatter () =
  let open Format in
  let fmt = formatter_of_out_channel stdout in
  (* Output is indented 4 spaces. 96+4 = 100 chars line length. *)
  pp_set_margin fmt 96;
  (* Nice round number, seems to work well by trial-and-error. *)
  pp_set_max_indent fmt 32;
  fmt

let parse_options ~module_ref_prefix ~module_ref_prefix_LEGACY_INTEROP =
  let open Parser_env in
  Some
    {
      default_parse_options with
      components = true;
      enums = true;
      module_ref_prefix;
      module_ref_prefix_LEGACY_INTEROP;
    }

let sig_options
    ?(suppress_types = SSet.empty)
    ?(munge = false)
    ?(facebook_keyMirror = false)
    ?facebook_fbt
    ?(max_literal_len = 100)
    ?(exact_by_default = false)
    ?(enable_enums = true)
    ?(enable_component_syntax = true)
    ?(enable_ts_syntax = true)
    ?(hooklike_functions = true)
    ?(enable_relay_integration = false)
    ?(casting_syntax = Options.CastingSyntax.Colon)
    ?relay_integration_module_prefix
    ?(for_builtins = false)
    ?(locs_to_dirtify = [])
    () =
  {
    Type_sig_options.suppress_types;
    munge;
    facebook_keyMirror;
    facebook_fbt;
    max_literal_len;
    exact_by_default;
    enable_enums;
    enable_component_syntax;
    enable_ts_syntax;
    hooklike_functions;
    enable_relay_integration;
    casting_syntax;
    relay_integration_module_prefix;
    for_builtins;
    locs_to_dirtify;
  }

let parse_and_pack_module ~parse_options ~strict sig_opts contents =
  let (ast, _errors) = Parser_flow.program ~parse_options contents in
  Type_sig_utils.parse_and_pack_module ~strict sig_opts None ast

let print_sig
    ?munge
    ?facebook_fbt
    ?facebook_keyMirror
    ?exact_by_default
    ?max_literal_len
    ?module_ref_prefix
    ?module_ref_prefix_LEGACY_INTEROP
    ?enable_enums
    ?enable_component_syntax
    ?enable_relay_integration
    ?relay_integration_module_prefix
    ?for_builtins
    ?locs_to_dirtify
    contents_indent =
  let contents = dedent_trim contents_indent in
  let sig_opts =
    sig_options
      ?munge
      ?facebook_fbt
      ?facebook_keyMirror
      ?exact_by_default
      ?max_literal_len
      ?enable_enums
      ?enable_component_syntax
      ?enable_relay_integration
      ?relay_integration_module_prefix
      ?for_builtins
      ?locs_to_dirtify
      ()
  in
  let parse_options = parse_options ~module_ref_prefix ~module_ref_prefix_LEGACY_INTEROP in
  let type_sig =
    parse_and_pack_module
      ~parse_options
      ~strict:true
      ~platform_availability_set:None
      sig_opts
      contents
  in
  let fmt = make_test_formatter () in
  pp_sig fmt type_sig

let print_builtins ordered_contents_indent =
  let ordered_asts =
    List.map
      (fun contents_indent ->
        let contents = dedent_trim contents_indent in
        let parse_options =
          parse_options ~module_ref_prefix:None ~module_ref_prefix_LEGACY_INTEROP:None
        in
        let (ast, _errors) = Parser_flow.program ~parse_options contents in
        ast)
      ordered_contents_indent
  in
  let sig_opts = sig_options ~for_builtins:true () in
  let builtins = Type_sig_utils.parse_and_pack_builtins sig_opts ordered_asts in
  let fmt = make_test_formatter () in
  pp_builtins fmt builtins

(* TODO: ocamlformat mangles the ppx syntax. *)
[@@@ocamlformat "disable=true"]

let%expect_test "export_number_literal" =
  print_sig {|
    export default 0;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14]; def = (Value (NumberLit ([1:15-16], 0., "0")))}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_bigint_literal" =
  print_sig {|
    export default 0n;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def = (Value (BigIntLit ([1:15-17], (Some 0L), "0n")))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_bigint_literal_neg" =
  print_sig {|
    export default -0n;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Eval ([1:15-18], (Value (BigIntLit ([1:16-18], (Some 0L), "0n"))),
             (Unary Flow_ast.Expression.Unary.Minus)))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_function_literal" =
  print_sig {|
    export default function(x: number): number { return x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-55];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Number [1:27-33]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Number [1:36-42]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_function_literal_check1" =
  print_sig {|
    export default function(x): number { return x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-47];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Err [1:24-25])}];
                 rest_param = None; this_param = None;
                 return = (Annot (Number [1:28-34]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Errors:
    (SigError (Signature_error.ExpectedAnnotation ([1:24-25], Expected_annotation_sort.Identifier)))
  |}]

let%expect_test "export_function_literal_check2" =
  print_sig {|
    export default function(x: number) { return x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-47];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Number [1:27-33]))}];
                 rest_param = None; this_param = None;
                 return = (Err [1:34]);
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:34], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "export_function_reference" =
  print_sig {|
    function foo(x: number): number { return x };
    export default foo;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def = (Ref LocalRef {ref_loc = [2:15-18]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:0-31];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Number [1:16-22]))}];
           rest_param = None; this_param = None;
           return = (Annot (Number [1:25-31]));
           predicate = None; hook = NonHook};
         statics = {}}
  |}]

let%expect_test "export_function_reference_check1" =
  print_sig {|
    function foo(x): number { return x };
    export default foo;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def = (Ref LocalRef {ref_loc = [2:15-18]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:0-23];
         def =
         FunSig {tparams = Mono; params = [FunParam {name = (Some "x"); t = (Err [1:13-14])}];
           rest_param = None; this_param = None;
           return = (Annot (Number [1:17-23]));
           predicate = None; hook = NonHook};
         statics = {}}

    Errors:
    (SigError (Signature_error.ExpectedAnnotation ([1:13-14], Expected_annotation_sort.Identifier)))
  |}]

let%expect_test "export_function_reference_check2" =
  print_sig {|
    function foo(x: number) { return x }
    export default foo;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def = (Ref LocalRef {ref_loc = [2:15-18]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:0-23];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Number [1:16-22]))}];
           rest_param = None; this_param = None;
           return = (Err [1:23]); predicate = None;
           hook = NonHook};
         statics = {}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:23], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "function_param_optional" =
  print_sig {|
    export default function(p?: string): void {};
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-44];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "p"); t = (Annot (Optional (Annot (String [1:28-34]))))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Void [1:37-41]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "function_param_default" =
  print_sig {|
    export default function(p: string = "foo"): void {};
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-51];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "p"); t = (Annot (Optional (Annot (String [1:27-33]))))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Void [1:44-48]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "function_param_default_check" =
  print_sig {|
    export default function(p = "foo") {}
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-37];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "p"); t = (Annot (Optional (Err [1:24-25])))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Void [1:34]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Errors:
    (SigError (Signature_error.ExpectedAnnotation ([1:24-25], Expected_annotation_sort.Identifier)))

  |}]

let%expect_test "function_param_typeof_reference" =
  print_sig {|
    declare const bar: string;
    export default function(bar: typeof bar, baz: typeof bar, {boz}: {boz: typeof baz}) {}
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             FunExpr {loc = [2:15-86];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "bar");
                    t =
                    (Annot
                       Typeof {loc = [2:29-39];
                         qname = ["bar"];
                         t = (Ref LocalRef {ref_loc = [2:36-39]; index = 0});
                         targs = None})};
                   FunParam {name = (Some "baz");
                     t =
                     (Annot
                        Typeof {loc = [2:46-56];
                          qname = ["bar"];
                          t = (Ref LocalRef {ref_loc = [2:53-56]; index = 1});
                          targs = None})};
                   FunParam {name = None;
                     t =
                     (Annot
                        ObjAnnot {loc = [2:65-82];
                          obj_kind = InexactObj;
                          props =
                          { "boz" ->
                            (ObjAnnotField ([2:66-69],
                               (Annot
                                  Typeof {
                                    loc = [2:71-81];
                                    qname = ["baz"];
                                    t = (Ref LocalRef {ref_loc = [2:78-81]; index = 2});
                                    targs = None}),
                               Polarity.Neutral)) };
                          proto = ObjAnnotImplicitProto})}
                   ];
                 rest_param = None; this_param = None;
                 return = (Annot (Void [2:83]));
                 predicate = None; hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:14-17]; name = "bar"; def = (Annot (String [1:19-25]))}
    1. Variable {id_loc = [2:24-27];
         name = "bar";
         def =
         (Annot
            Typeof {loc = [2:29-39];
              qname = ["bar"]; t = (Ref LocalRef {ref_loc = [2:36-39]; index = 0});
              targs = None})}
    2. Variable {id_loc = [2:41-44];
         name = "baz";
         def =
         (Annot
            Typeof {loc = [2:46-56];
              qname = ["bar"]; t = (Ref LocalRef {ref_loc = [2:53-56]; index = 1});
              targs = None})}

  |}]

let%expect_test "component_param_typeof_reference" =
  print_sig {|
    declare const bar: string;
    export component C(bar: typeof bar, baz: typeof bar, booz as {boz}: {boz: typeof baz}) {}
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"C"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:14-17]; name = "bar"; def = (Annot (String [1:19-25]))}
    1. ComponentBinding {id_loc = [2:17-18];
         name = "C"; fn_loc = [2:7-86];
         def =
         ComponentSig {params_loc = [2:18-86];
           tparams = Mono;
           params =
           [ComponentParam {name = "bar";
              name_loc = [2:19-22];
              t =
              (Annot
                 Typeof {loc = [2:24-34];
                   qname = ["bar"]; t = (Ref LocalRef {ref_loc = [2:31-34]; index = 0});
                   targs = None})};
             ComponentParam {name = "baz";
               name_loc = [2:36-39];
               t =
               (Annot
                  Typeof {loc = [2:41-51];
                    qname = ["bar"];
                    t = (Ref LocalRef {ref_loc = [2:48-51]; index = 0});
                    targs = None})};
             ComponentParam {name = "booz";
               name_loc = [2:53-57];
               t =
               (Annot
                  ObjAnnot {loc = [2:68-85];
                    obj_kind = InexactObj;
                    props =
                    { "boz" ->
                      (ObjAnnotField ([2:69-72],
                         (Annot
                            Typeof {loc = [2:74-84];
                              qname = ["baz"];
                              t =
                              (Ref BuiltinRef {ref_loc = [2:81-84]; type_ref = false; name = "baz"});
                              targs = None}),
                         Polarity.Neutral)) };
                    proto = ObjAnnotImplicitProto})}
             ];
           rest_param = None;
           renders =
           (Annot
              (Renders ([2:86],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [2:86]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}

  |}]

let%expect_test "export_object_literal_property_literal" =
  print_sig {|
    export default { p: 0 };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             ObjLit {loc = [1:15-23];
               frozen = false; proto = None;
               props =
               { "p" ->
                 (ObjValueField ([1:17-18], (
                    Value (NumberLit ([1:20-21], 0., "0"))), Polarity.Neutral)) }})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_object_literal_property_reference" =
  print_sig {|
    var x: number = 0;
    export default { p: x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             ObjLit {loc = [2:15-23];
               frozen = false; proto = None;
               props =
               { "p" ->
                 (ObjValueField ([2:17-18], (
                    Ref LocalRef {ref_loc = [2:20-21]; index = 0}), Polarity.Neutral)) }})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "x"; def = (Annot (Number [1:7-13]))}
  |}]

let%expect_test "export_object_literal_property_reference_check" =
  print_sig {|
    var x = 0;
    export default { p: x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             ObjLit {loc = [2:15-23];
               frozen = false; proto = None;
               props =
               { "p" ->
                 (ObjValueField ([2:17-18], (
                    Ref LocalRef {ref_loc = [2:20-21]; index = 0}), Polarity.Neutral)) }})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "x"; def = (Err [1:4-5])}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:4-5],
          Expected_annotation_sort.VariableDefinition {name = "x"})))
  |}]

let%expect_test "empty_object_literal" =
  print_sig {|
    export default { };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def = (Value ObjLit {loc = [1:15-18]; frozen = false; proto = None; props = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}
  |}]

let%expect_test "export_class_reference" =
  print_sig {|
    class C {
      f: number = 0;
      m(x: number): number { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-36]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Number [3:7-13]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Number [3:16-22]));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" -> (ObjValueField ([2:2-3], (Annot (Number [2:5-11])), Polarity.Neutral)) }}}
  |}]

let%expect_test "export_class_reference_check1" =
  print_sig {|
    class C {
      f = 0;
      m(x: number): number { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-36]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Number [3:7-13]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Number [3:16-22]));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([2:2-3], (Value (NumberLit ([2:6-7], 0., "0"))), Polarity.Neutral)) }}}
  |}]

let%expect_test "export_class_reference_check2" =
  print_sig {|
    class C {
      f: number = 0;
      m(x): number { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-28]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Err [3:4-5])}];
                 rest_param = None; this_param = None;
                 return = (Annot (Number [3:8-14]));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" -> (ObjValueField ([2:2-3], (Annot (Number [2:5-11])), Polarity.Neutral)) }}}

    Errors:
    (SigError (Signature_error.ExpectedAnnotation ([3:4-5], Expected_annotation_sort.Identifier)))
  |}]

let%expect_test "export_class_reference_check3" =
  print_sig {|
    class C {
      f: number = 0;
      m(x: number) { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-28]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Number [3:7-13]))}];
                 rest_param = None; this_param = None;
                 return = (Err [3:14]);
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" -> (ObjValueField ([2:2-3], (Annot (Number [2:5-11])), Polarity.Neutral)) }}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([3:14], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "type_alias_dependencies" =
  print_sig {|
    type T1 = number;
    type T2 = number;
    type T3 = number;
    class C {
      f: T1 = 0;
      m(x: T2): T3 { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [8:7-14];
          def = (Ref LocalRef {ref_loc = [8:15-16]; index = 3})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-7]; name = "T1"; tparams = Mono; body = (Annot (Number [1:10-16]))}
    1. TypeAlias {id_loc = [2:5-7]; name = "T2"; tparams = Mono; body = (Annot (Number [2:10-16]))}
    2. TypeAlias {id_loc = [3:5-7]; name = "T3"; tparams = Mono; body = (Annot (Number [3:10-16]))}
    3. ClassBinding {id_loc = [4:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [6:2-3];
               fn_loc = [6:2-28]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [6:7-9]; index = 1}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [6:12-14]; index = 2}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([5:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [5:5-7]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "class_dependencies" =
  print_sig {|
    class D { f: number = 0; }
    class C {
      f: D = new D;
      m(x: D): D { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (Ref LocalRef {ref_loc = [6:15-16]; index = 1})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "D";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" -> (ObjValueField ([1:10-11], (Annot (Number [1:13-19])), Polarity.Neutral)) }}}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [4:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [4:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([3:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [3:5-6]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "class_dependencies_check" =
  print_sig {|
    class D { f = 0; }
    class C {
      f: D = new D;
      m(x: D): D { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (Ref LocalRef {ref_loc = [6:15-16]; index = 1})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "D";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" ->
             (ObjValueField ([1:10-11], (
                Value (NumberLit ([1:14-15], 0., "0"))), Polarity.Neutral)) }}}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [4:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [4:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([3:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [3:5-6]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "export_new_typecast" =
  print_sig {|
    class D { f: number = 0; }
    class C {
      f: D = new D;
      m(x: D): D { return x; }
    }
    export default (new C: C);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (TyRef (Unqualified LocalRef {ref_loc = [6:23-24]; index = 1}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "D";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" -> (ObjValueField ([1:10-11], (Annot (Number [1:13-19])), Polarity.Neutral)) }}}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [4:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [4:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([3:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [3:5-6]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "export_new_typecast_check" =
  print_sig {|
    class D { f = 0; }
    class C {
      f: D = new D;
      m(x: D): D { return x; }
    }
    export default (new C: C);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (TyRef (Unqualified LocalRef {ref_loc = [6:23-24]; index = 1}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "D";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" ->
             (ObjValueField ([1:10-11], (
                Value (NumberLit ([1:14-15], 0., "0"))), Polarity.Neutral)) }}}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [4:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [4:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([3:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [3:5-6]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "recursive_dependencies" =
  print_sig {|
    class C {
      f: C = new C;
      m(x: C): C { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [3:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [3:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([2:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [2:5-6]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "recursive_dependencies_check" =
  print_sig {|
    class C {
      f = new C;
      m(x: C): C { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (Ref LocalRef {ref_loc = [5:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [3:2-3];
               fn_loc = [3:2-26]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified LocalRef {ref_loc = [3:7-8]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified LocalRef {ref_loc = [3:11-12]; index = 0}));
                 predicate = None; hook = NonHook}} };
           own_props = { "f" -> (ObjValueField ([2:2-3], (Err [2:6-11]), Polarity.Neutral)) }}}

    Errors:
    (SigError (Signature_error.UnexpectedExpression ([2:6-11], Flow_ast_utils.ExpressionSort.New)))
  |}]

let%expect_test "typeof_dependencies" =
  print_sig {|
    var x: number = 0;
    class C {
      p: typeof x = 0;
    }
    export default (new C: C);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (TyRef (Unqualified LocalRef {ref_loc = [5:23-24]; index = 1}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "x"; def = (Annot (Number [1:7-13]))}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "p" ->
             (ObjValueField ([3:2-3],
                (Annot
                   Typeof {loc = [3:5-13];
                     qname = ["x"]; t = (Ref LocalRef {ref_loc = [3:12-13]; index = 0});
                     targs = None}),
                Polarity.Neutral)) }}}
  |}]

let%expect_test "typeof_dependencies_check" =
  print_sig {|
    var x = 0;
    class C {
      p: typeof x = 0;
    }
    export default (new C: C);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (TyRef (Unqualified LocalRef {ref_loc = [5:23-24]; index = 1}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "x"; def = (Err [1:4-5])}
    1. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "p" ->
             (ObjValueField ([3:2-3],
                (Annot
                   Typeof {loc = [3:5-13];
                     qname = ["x"]; t = (Ref LocalRef {ref_loc = [3:12-13]; index = 0});
                     targs = None}),
                Polarity.Neutral)) }}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:4-5],
          Expected_annotation_sort.VariableDefinition {name = "x"})))
  |}]

let%expect_test "const_initializer" =
  print_sig {|
    const x = 0;
    export default { x };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             ObjLit {loc = [2:15-20];
               frozen = false; proto = None;
               props =
               { "x" ->
                 (ObjValueField ([2:17-18], (
                    Ref LocalRef {ref_loc = [2:17-18]; index = 0}), Polarity.Neutral)) }})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "x"; def = (Value (NumberLit ([1:10-11], 0., "0")))}
  |}]

let%expect_test "empty_array_literal" =
  print_sig {|
    export default [ ];
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefault {default_loc = [1:7-14]; def = (Err [1:15-18])}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Errors:
    (SigError (Signature_error.EmptyArray [1:15-18]))
  |}]

let%expect_test "non_empty_array_literal" =
  print_sig {|
    const x = 0;
    var y = false;
    export default [ x, y ];
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [3:7-14];
          def =
          (Value
             (ArrayLit ([3:15-23], (Ref LocalRef {ref_loc = [3:17-18]; index = 0}),
                [(Ref LocalRef {ref_loc = [3:20-21]; index = 1})])))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "x"; def = (Value (NumberLit ([1:10-11], 0., "0")))}
    1. Variable {id_loc = [2:4-5]; name = "y"; def = (Err [2:4-5])}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([2:4-5],
          Expected_annotation_sort.VariableDefinition {name = "y"})))
  |}]

let%expect_test "void_function" =
  print_sig {|
    function foo() {}
    export default foo;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def = (Ref LocalRef {ref_loc = [2:15-18]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:0-14];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:14]));
           predicate = None; hook = NonHook};
         statics = {}}
  |}]

let%expect_test "void_generator" =
  print_sig {|
    function* foo() { yield 0; }
    export default foo;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def = (Ref LocalRef {ref_loc = [2:15-18]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:10-13];
         name = "foo"; async = false;
         generator = true; fn_loc = [1:0-15];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Err [1:15]); predicate = None;
           hook = NonHook};
         statics = {}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:15], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "import_default_dependencies" =
  print_sig {|
    import x from './import_default_dependencies_helper';
    class C {
      p: typeof x = 0;
    }
    export default (new C: C);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [5:7-14];
          def = (TyRef (Unqualified LocalRef {ref_loc = [5:23-24]; index = 0}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. ./import_default_dependencies_helper

    Local defs:
    0. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "p" ->
             (ObjValueField ([3:2-3],
                (Annot
                   Typeof {loc = [3:5-13];
                     qname = ["x"]; t = (Ref RemoteRef {ref_loc = [3:12-13]; index = 0});
                     targs = None}),
                Polarity.Neutral)) }}}

    Remote refs:
    0. Import {id_loc = [1:7-8]; name = "x"; index = 0; remote = "default"}
  |}]

let%expect_test "import_type_dependencies" =
  print_sig {|
    import type { T1, T2, T3 } from './import_type_dependencies_helper';
    class C {
      f: T1 = 0;
      m(x: T2): T3 { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (Ref LocalRef {ref_loc = [6:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. ./import_type_dependencies_helper

    Local defs:
    0. ClassBinding {id_loc = [2:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-28]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (TyRef (Unqualified RemoteRef {ref_loc = [4:7-9]; index = 1}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (TyRef (Unqualified RemoteRef {ref_loc = [4:12-14]; index = 2}));
                 predicate = None; hook = NonHook}} };
           own_props =
           { "f" ->
             (ObjValueField ([3:2-3],
                (TyRef (Unqualified RemoteRef {ref_loc = [3:5-7]; index = 0})), Polarity.Neutral)) }}}

    Remote refs:
    0. ImportType {id_loc = [1:14-16]; name = "T1"; index = 0; remote = "T1"}
    1. ImportType {id_loc = [1:18-20]; name = "T2"; index = 0; remote = "T2"}
    2. ImportType {id_loc = [1:22-24]; name = "T3"; index = 0; remote = "T3"}
  |}]

let%expect_test "qualified_references" =
  print_sig {|
    import M1 from './qualified_references_helper';
    import type M2 from './qualified_references_helper';
    class C {
      m(x: M1.T): M2.T { return x; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [6:7-14];
          def = (Ref LocalRef {ref_loc = [6:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. ./qualified_references_helper

    Local defs:
    0. ClassBinding {id_loc = [3:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [4:2-3];
               fn_loc = [4:2-32]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t =
                    (TyRef
                       Qualified {loc = [4:7-11];
                         id_loc = [4:10-11];
                         name = "T";
                         qualification = (Unqualified RemoteRef {ref_loc = [4:7-9]; index = 0})})}
                   ];
                 rest_param = None; this_param = None;
                 return =
                 (TyRef
                    Qualified {loc = [4:14-18];
                      id_loc = [4:17-18];
                      name = "T";
                      qualification = (Unqualified RemoteRef {ref_loc = [4:14-16]; index = 1})});
                 predicate = None; hook = NonHook}} };
           own_props = {}}}

    Remote refs:
    0. Import {id_loc = [1:7-9]; name = "M1"; index = 0; remote = "default"}
    1. ImportType {id_loc = [2:12-14]; name = "M2"; index = 0; remote = "default"}
  |}]

let%expect_test "invalid_qualified_references" =
  print_sig {|
    export type T<U> = U.V;
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "T";
         tparams =
         (Poly ([1:13-16],
            TParam {name_loc = [1:14-15];
              name = "U"; polarity = Polarity.Neutral;
              bound = None; default = None},
            []));
         body = (Err [1:19-20])}

    Errors:
    CheckError |}]

let%expect_test "hoisted_requires" =
  print_sig {|
    const M = require('./hoisted_requires_helper');
    if (Math.random() < 0.5) {
      var { D } = require('./hoisted_requires_helper');
    } else {
      var { D } = require('./hoisted_requires_helper');
    }
    var D = 0;
    class C extends M.D {
      f: D = 0;
    }
    module.exports = C;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [11:17-18]; index = 2}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. ./hoisted_requires_helper

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "M"; def = Require {loc = [1:10-46]; index = 0}}
    1. Variable {id_loc = [3:8-9]; name = "D"; def = (Pattern 1)}
    2. ClassBinding {id_loc = [8:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [8:16-19];
             t = (Eval ([8:16-19], (Ref LocalRef {ref_loc = [8:16-17]; index = 0}), (GetProp "D")))};
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" ->
             (ObjValueField ([9:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [9:5-6]; index = 1})), Polarity.Neutral)) }}}

    Pattern defs:
    0. (Err [3:11])

    Patterns:
    0. (PDef 0)
    1. PropP {id_loc = [3:8-9]; name = "D"; def = 0}

    Errors:
    (SigError (Signature_error.ExpectedAnnotation ([3:11], Expected_annotation_sort.ArrayPattern)))
  |}]

let%expect_test "hoisted_locals" =
  print_sig {|
    const M = require('./hoisted_locals_helper');
    if (Math.random() < 0.5) {
      var D = 0;
    } else {
      var D = false;
    }
    class C extends M.D {
      f: D = 0;
    }
    module.exports = C;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [10:17-18]; index = 2}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. ./hoisted_locals_helper

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "M"; def = Require {loc = [1:10-44]; index = 0}}
    1. Variable {id_loc = [3:6-7]; name = "D"; def = (Err [3:6-7])}
    2. ClassBinding {id_loc = [7:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [7:16-19];
             t = (Eval ([7:16-19], (Ref LocalRef {ref_loc = [7:16-17]; index = 0}), (GetProp "D")))};
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" ->
             (ObjValueField ([8:2-3],
                (TyRef (Unqualified LocalRef {ref_loc = [8:5-6]; index = 1})), Polarity.Neutral)) }}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([3:6-7],
          Expected_annotation_sort.VariableDefinition {name = "D"})))
  |}]

let%expect_test "dynamic_requires" =
  print_sig {|
    module.exports = require('./dynamic_requires_helper');
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some Require {loc = [1:17-53]; index = 0});
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. ./dynamic_requires_helper
  |}]

let%expect_test "import_dynamic" =
  print_sig {|
    module.exports = import('foo');
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some ImportDynamic {loc = [1:17-30]; index = 0});
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. foo |}]

let%expect_test "enable_relay_integration" =
  print_sig ~enable_relay_integration:true {|
    module.exports = graphql`query foo {}`;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some Require {loc = [1:17-38]; index = 0});
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. foo.graphql |}]

let%expect_test "relay_integration_module_prefix" =
  print_sig ~enable_relay_integration:true ~relay_integration_module_prefix:"./__generated__/" {|
    module.exports = graphql`query foo {}`;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some Require {loc = [1:17-38]; index = 0});
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. ./__generated__/foo.graphql |}]

let%expect_test "scope_extrusion" =
  print_sig {|
    {
      class C {}
      var x: C = new C;
    }
    class C {
      f = 0;
    }
    module.exports = x;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [8:17-18]; index = 1}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [2:8-9];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
    1. Variable {id_loc = [3:6-7]; name = "x";
         def = (TyRef (Unqualified LocalRef {ref_loc = [3:9-10]; index = 0}))}
  |}]

let%expect_test "scope_extrusion_nested" =
  print_sig {|
    {
      class C {}
      let y = 0;
      if (b) {
        var x: C = new C;
      }
    }
    class C {
      f = 0;
    }
    module.exports = { x, y };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [11:17-25];
                 frozen = false; proto = None;
                 props =
                 { "x" ->
                   (ObjValueField ([11:19-20], (
                      Ref LocalRef {ref_loc = [11:19-20]; index = 1}), Polarity.Neutral));
                   "y" ->
                   (ObjValueField ([11:22-23],
                      (Ref BuiltinRef {ref_loc = [11:22-23]; type_ref = false; name = "y"}),
                      Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [2:8-9];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
    1. Variable {id_loc = [5:8-9]; name = "x";
         def = (TyRef (Unqualified LocalRef {ref_loc = [5:11-12]; index = 0}))}
  |}]

let%expect_test "report_all_errors" =
  print_sig {|
    class A {
      f = (x: number) => x;
    }
    module.exports = {
      a: A,
      b: (x: string) => x,
    };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [4:17-7:1];
                 frozen = false; proto = None;
                 props =
                 { "a" ->
                   (ObjValueField ([5:2-3], (
                      Ref LocalRef {ref_loc = [5:5-6]; index = 0}), Polarity.Neutral));
                   "b" ->
                   (ObjValueField ([6:2-3],
                      (Value
                         FunExpr {loc = [6:5-21];
                           async = false;
                           generator = false;
                           def =
                           FunSig {tparams = Mono;
                             params = [FunParam {name = (Some "x"); t = (Annot (String [6:9-15]))}];
                             rest_param = None;
                             this_param = None;
                             return = (Err [6:16]);
                             predicate = None;
                             hook = NonHook};
                           statics = {}}),
                      Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "A";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "f" ->
             (ObjValueField ([2:2-3],
                (Value
                   FunExpr {loc = [2:6-22];
                     async = false; generator = false;
                     def =
                     FunSig {tparams = Mono;
                       params = [FunParam {name = (Some "x"); t = (Annot (Number [2:10-16]))}];
                       rest_param = None;
                       this_param = None;
                       return = (Err [2:17]);
                       predicate = None;
                       hook = NonHook};
                     statics = {}}),
                Polarity.Neutral)) }}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([6:16], Expected_annotation_sort.FunctionReturn)))
    (SigError
       (Signature_error.ExpectedAnnotation ([2:17], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "munged_methods_ignored_if_directive" =
  print_sig ~munge:true {|
    class C {
      _method() { return 1; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:7-14];
          def = (Ref LocalRef {ref_loc = [4:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
  |}]

let%expect_test "munged_methods_not_ignored" =
  print_sig {|
    class C {
      _method() { return 1; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:7-14];
          def = (Ref LocalRef {ref_loc = [4:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "_method" ->
             ObjValueMethod {id_loc = [2:2-9];
               fn_loc = [2:2-25]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = []; rest_param = None;
                 this_param = None; return = (Err [2:11]);
                 predicate = None; hook = NonHook}} };
           own_props = {}}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([2:11], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "munged_fields_ignored_if_directive" =
  print_sig ~munge:true {|
    class C {
      _method = () => { return 1; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:7-14];
          def = (Ref LocalRef {ref_loc = [4:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
  |}]

let%expect_test "munged_fields_not_ignored" =
  print_sig {|
    class C {
      _method = () => { return 1; }
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:7-14];
          def = (Ref LocalRef {ref_loc = [4:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "_method" ->
             (ObjValueField ([2:2-9],
                (Value
                   FunExpr {loc = [2:12-31];
                     async = false; generator = false;
                     def =
                     FunSig {tparams = Mono;
                       params = []; rest_param = None;
                       this_param = None;
                       return = (Err [2:14]);
                       predicate = None;
                       hook = NonHook};
                     statics = {}}),
                Polarity.Neutral)) }}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([2:14], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "propTypes_static_failure" =
  print_sig {|
    class C {
      static propTypes = {}
    }
    export default C;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:7-14];
          def = (Ref LocalRef {ref_loc = [4:15-16]; index = 0})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = [];
           static_props =
           { "propTypes" ->
             (ObjValueField ([2:9-18],
                (Value ObjLit {loc = [2:21-23]; frozen = false; proto = None; props = {}}),
                Polarity.Neutral)) };
           proto_props = {}; own_props = {}}}
  |}]

let%expect_test "array_spread" =
  print_sig {|
    module.exports = [1, ...[2, 3], 4];
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some (Err [1:17-34]));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Errors:
    (SigError (Signature_error.UnexpectedArraySpread ([1:17-34], [1:21-30])))
  |}]

let%expect_test "array_hole" =
  print_sig {|
    module.exports = [,];
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some (Err [1:17-20]));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Errors:
    (SigError (Signature_error.UnexpectedArrayHole [1:17-20]))
  |}]

let%expect_test "object_spread" =
  print_sig {|
    module.exports = { x: 'x', ...{ y: 'y' }, z: 'z' };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjSpreadLit {loc = [1:17-50];
                 frozen = false; proto = None;
                 elems_rev =
                 ((ObjValueSpreadSlice
                     { "z" ->
                       (ObjValueField ([1:42-43], (
                          Value (StringLit ([1:45-48], "z"))), Polarity.Neutral)) }),
                  [(ObjValueSpreadElem
                      (Value
                         ObjLit {loc = [1:30-40];
                           frozen = false;
                           proto = None;
                           props =
                           { "y" ->
                             (ObjValueField ([1:32-33], (
                                Value (StringLit ([1:35-38], "y"))), Polarity.Neutral)) }}));
                    (ObjValueSpreadSlice
                       { "x" ->
                         (ObjValueField ([1:19-20], (
                            Value (StringLit ([1:22-25], "x"))), Polarity.Neutral)) })
                    ])}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "reference_expression1" =
  print_sig {|
    module.exports = Number.NaN;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Eval ([1:17-27],
               (Ref BuiltinRef {ref_loc = [1:17-23]; type_ref = false; name = "Number"}),
               (GetProp "NaN"))));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "reference_expression2" =
  print_sig {|
    module.exports = 'x'.length;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some (Err [1:17-27]));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Errors:
    (SigError
       (Signature_error.UnexpectedExpression ([1:17-27], Flow_ast_utils.ExpressionSort.Member)))
  |}]

let%expect_test "member_expression" =
  print_sig {|
    module.exports = a[0];
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Eval ([1:17-21], (Ref BuiltinRef {ref_loc = [1:17-18]; type_ref = false; name = "a"}),
               (GetElem (Value (NumberLit ([1:19-20], 0., "0")))))));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "arith_expression1" =
  print_sig {|
    module.exports = 6*7;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Eval ([1:17-20], (Value (NumberLit ([1:17-18], 6., "6"))),
               (Arith (Flow_ast.Expression.Binary.Mult, (Value (NumberLit ([1:19-20], 7., "7")))))
               )));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "arith_expression2" =
  print_sig {|
    module.exports = 6+7;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Eval ([1:17-20], (Value (NumberLit ([1:17-18], 6., "6"))),
               (Arith (Flow_ast.Expression.Binary.Plus, (Value (NumberLit ([1:19-20], 7., "7")))))
               )));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "update_expression" =
  print_sig {|
    import {foo} from 'bar';
    export const pre_incr = ++foo;
    export const pre_decr = --foo;
    export const post_incr = foo++;
    export const post_decr = foo--;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 3); (ExportBinding 2); (ExportBinding 1); (ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"post_decr"; "post_incr"; "pre_decr"; "pre_incr"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. bar

    Local defs:
    0. Variable {id_loc = [2:13-21];
         name = "pre_incr";
         def = (Eval ([2:24-29], (Ref RemoteRef {ref_loc = [2:26-29]; index = 0}), Update))}
    1. Variable {id_loc = [3:13-21];
         name = "pre_decr";
         def = (Eval ([3:24-29], (Ref RemoteRef {ref_loc = [3:26-29]; index = 0}), Update))}
    2. Variable {id_loc = [4:13-22];
         name = "post_incr";
         def = (Eval ([4:25-30], (Ref RemoteRef {ref_loc = [4:25-28]; index = 0}), Update))}
    3. Variable {id_loc = [5:13-22];
         name = "post_decr";
         def = (Eval ([5:25-30], (Ref RemoteRef {ref_loc = [5:25-28]; index = 0}), Update))}

    Remote refs:
    0. Import {id_loc = [1:8-11]; name = "foo"; index = 0; remote = "foo"} |}]

let%expect_test "sequence_expression" =
  print_sig {|
    var x;
    export default (x, null);
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefault {default_loc = [2:7-14]; def = (Value (NullLit [2:19-23]))}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "named_class_expression" =
  print_sig {|
    module.exports = class C { };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [1:23-24]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:23-24];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
  |}]

let%expect_test "named_function_expression" =
  print_sig {|
    module.exports = function foo() { };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [1:26-29]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:26-29];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:17-31];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:31]));
           predicate = None; hook = NonHook};
         statics = {}}
  |}]

let%expect_test "interface_coverage" =
  print_sig {|
    declare interface Foo<X> { }
    declare export class C {
      foo: Foo<any>;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"C"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Interface {id_loc = [1:18-21];
         name = "Foo";
         tparams =
         (Poly ([1:21-24],
            TParam {name_loc = [1:22-23];
              name = "X"; polarity = Polarity.Neutral;
              bound = None; default = None},
            []));
         def = InterfaceSig {extends = []; props = {}; calls = []; dict = None}}
    1. DeclareClassBinding {id_loc = [2:21-22];
         name = "C";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {};
           own_props =
           { "foo" ->
             (InterfaceField ((Some [3:2-5]),
                TyRefApp {loc = [3:7-15];
                  name = (Unqualified LocalRef {ref_loc = [3:7-10]; index = 0});
                  targs = [(Annot (Any [3:11-14]))]},
                Polarity.Neutral)) };
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}}
  |}]

let%expect_test "bound_coverage" =
  print_sig {|
    type Foo = number;
    export type T = <X: Foo> (X) => void;
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 1)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-8]; name = "Foo"; tparams = Mono; body = (Annot (Number [1:11-17]))}
    1. TypeAlias {id_loc = [2:12-13];
         name = "T"; tparams = Mono;
         body =
         (Annot
            (FunAnnot ([2:16-36],
               FunSig {
                 tparams =
                 (Poly ([2:16-24],
                    TParam {name_loc = [2:17-18];
                      name = "X"; polarity = Polarity.Neutral;
                      bound =
                      (Some (TyRef (Unqualified LocalRef {ref_loc = [2:20-23]; index = 0})));
                      default = None},
                    []));
                 params =
                 [FunParam {name = None; t = (Annot Bound {ref_loc = [2:26-27]; name = "X"})}];
                 rest_param = None; this_param = None;
                 return = (Annot (Void [2:32-36]));
                 predicate = None; hook = NonHook}
               )))}
  |}]

let%expect_test "recursive_class_coverage" =
  print_sig {|
    module.exports = class C { x: C; };
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [1:23-24]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:23-24];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "x" ->
             (ObjValueField ([1:27-28],
                (TyRef (Unqualified LocalRef {ref_loc = [1:30-31]; index = 0})), Polarity.Neutral)) }}}
  |}]

let%expect_test "shadowed_class_expression" =
  print_sig {|
    class C { }
    module.exports = class C { }
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [2:23-24]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [2:23-24];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
  |}]

let%expect_test "frozen_object" =
  print_sig {|
    module.exports = Object.freeze({ foo: 42, bar: 'hello' });
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [1:31-56];
                 frozen = true; proto = None;
                 props =
                 { "bar" ->
                   (ObjValueField ([1:42-45], (
                      Value (StringLit ([1:47-54], "hello"))), Polarity.Neutral));
                   "foo" ->
                   (ObjValueField ([1:33-36], (
                      Value (NumberLit ([1:38-40], 42., "42"))), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "frozen_object_empty" =
  print_sig {|
    module.exports = Object.freeze({});
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Value ObjLit {loc = [1:31-33]; frozen = true; proto = None; props = {}}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "fbt_empty_open_close" =
  print_sig ~facebook_fbt:"FbtElement" {|
    module.exports = <fbt></fbt>;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (TyRef
               (Unqualified BuiltinRef {ref_loc = [1:18-21]; type_ref = true; name = "FbtElement"})));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "fbt_empty_open" =
  print_sig ~facebook_fbt:"FbtElement" {|
    module.exports = <fbt/>;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (TyRef
               (Unqualified BuiltinRef {ref_loc = [1:18-21]; type_ref = true; name = "FbtElement"})));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "fbt_with_child" =
  print_sig ~facebook_fbt:"FbtElement" {|
    function foo() {}
    module.exports = <fbt desc={foo()}></fbt>;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (TyRef
               (Unqualified BuiltinRef {ref_loc = [2:18-21]; type_ref = true; name = "FbtElement"})));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "keymirror" =
  print_sig ~facebook_keyMirror:true {|
    module.exports = keyMirror({
      a: null,
      b: null,
    })
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [1:27-4:1];
                 frozen = false; proto = None;
                 props =
                 { "a" ->
                   (ObjValueField ([2:2-3], (
                      Annot (SingletonString ([2:2-3], "a"))), Polarity.Neutral));
                   "b" ->
                   (ObjValueField ([3:2-3], (
                      Annot (SingletonString ([3:2-3], "b"))), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "jsx_div" =
  print_sig {|
    module.exports = <div></div>;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = (Some (Err [1:17-28]));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Errors:
    (SigError
       (Signature_error.UnexpectedExpression ([1:17-28], Flow_ast_utils.ExpressionSort.JSXElement)))
  |}]

let%expect_test "function_return" =
  print_sig {|
    var n = false;
    export function foo<X: typeof n>(x: X) { return 1; };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "n"; def = (Err [1:4-5])}
    1. FunBinding {id_loc = [2:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [2:7-38];
         def =
         FunSig {
           tparams =
           (Poly ([2:19-32],
              TParam {name_loc = [2:20-21];
                name = "X"; polarity = Polarity.Neutral;
                bound =
                (Some (Annot
                         Typeof {loc = [2:23-31];
                           qname = ["n"];
                           t = (Ref LocalRef {ref_loc = [2:30-31]; index = 0});
                           targs = None}));
                default = None},
              []));
           params =
           [FunParam {name = (Some "x"); t = (Annot Bound {ref_loc = [2:36-37]; name = "X"})}];
           rest_param = None; this_param = None;
           return = (Err [2:38]); predicate = None;
           hook = NonHook};
         statics = {}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([2:38], Expected_annotation_sort.FunctionReturn)))
    (SigError
       (Signature_error.ExpectedAnnotation ([1:4-5],
          Expected_annotation_sort.VariableDefinition {name = "n"})))
  |}]

let%expect_test "function_return_2" =
  print_sig {|
    var n = false;
    export function bar(x: (typeof n) => void) { return 1; };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"bar"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:4-5]; name = "n"; def = (Err [1:4-5])}
    1. FunBinding {id_loc = [2:16-19];
         name = "bar"; async = false;
         generator = false; fn_loc = [2:7-42];
         def =
         FunSig {tparams = Mono;
           params =
           [FunParam {name = (Some "x");
              t =
              (Annot
                 (FunAnnot ([2:23-41],
                    FunSig {tparams = Mono;
                      params =
                      [FunParam {name = None;
                         t =
                         (Annot
                            Typeof {loc = [2:24-32];
                              qname = ["n"];
                              t = (Ref LocalRef {ref_loc = [2:31-32]; index = 0});
                              targs = None})}
                        ];
                      rest_param = None;
                      this_param = None;
                      return = (Annot (Void [2:37-41]));
                      predicate = None;
                      hook = NonHook}
                    )))}
             ];
           rest_param = None; this_param = None;
           return = (Err [2:42]); predicate = None;
           hook = NonHook};
         statics = {}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([2:42], Expected_annotation_sort.FunctionReturn)))
    (SigError
       (Signature_error.ExpectedAnnotation ([1:4-5],
          Expected_annotation_sort.VariableDefinition {name = "n"})))
  |}]

let%expect_test "function_statics" =
  print_sig {|
    function bar(): void { };
    const x = 42;
    bar.x = x;
    module.exports = bar;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [4:17-20]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "bar"; async = false;
         generator = false; fn_loc = [1:0-20];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:16-20]));
           predicate = None; hook = NonHook};
         statics = { "x" -> ([3:4-5], (Ref LocalRef {ref_loc = [3:8-9]; index = 1})) }}
    1. Variable {id_loc = [2:6-7]; name = "x"; def = (Value (NumberLit ([2:10-12], 42., "42")))}
  |}]

let%expect_test "function_statics_conditional" =
  print_sig {|
    export function bar(): void { };
    declare var b: boolean;
    if (b) {
      bar.x = 42;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"bar"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:16-19];
         name = "bar"; async = false;
         generator = false; fn_loc = [1:7-27];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:23-27]));
           predicate = None; hook = NonHook};
         statics = {}}
  |}]

let%expect_test "function_predicates_1" =
  print_sig {|
    class A {};
    export function foo(x: mixed): boolean %checks {
      return x === new A;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [2:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [2:7-46];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [2:23-28]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:31-38]));
           predicate = (Some (Predicate ([3:2-21], None)));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "function_predicates_2" =
  print_sig {|
    declare function bar(x: mixed): boolean %checks(x === null);
    export function foo(x: mixed): boolean %checks {
      return bar(x);
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareFun {id_loc = [1:17-20];
         name = "bar"; fn_loc = [1:20-39];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:24-29]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [1:32-39]));
           predicate = (Some (Predicate ([1:40-59], (Some (NullP "x")))));
           hook = NonHook};
         tail = []}
    1. FunBinding {id_loc = [2:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [2:7-46];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [2:23-28]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:31-38]));
           predicate =
           (Some (Predicate ([3:2-16],
                    (Some (LatentP ((
                             Ref LocalRef {ref_loc = [3:9-12]; index = 0}), None,
                             [(Arg
                                 (Ref
                                    BuiltinRef {ref_loc = [3:13-14]; type_ref = false; name = "x"}))
                               ],
                             (("x", 0), []))))
                    )));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "function_predicates_3" =
  print_sig {|
    function bar(x: mixed): %checks { return x === null; }
    declare export function foo(x: mixed): boolean %checks(bar(x));
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "bar"; async = false;
         generator = false; fn_loc = [1:0-31];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:16-21]))}];
           rest_param = None; this_param = None;
           return = (Err [1:22]); predicate = (Some (Predicate ([1:34-52], (Some (NullP "x")))));
           hook = NonHook};
         statics = {}}
    1. DeclareFun {id_loc = [2:24-27];
         name = "foo"; fn_loc = [2:27-46];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [2:31-36]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:39-46]));
           predicate =
           (Some (Predicate ([2:47-62],
                    (Some (LatentP ((
                             Ref LocalRef {ref_loc = [2:55-58]; index = 0}), None,
                             [(Arg
                                 (Ref
                                    BuiltinRef {ref_loc = [2:59-60]; type_ref = false; name = "x"}))
                               ],
                             (("x", 0), []))))
                    )));
           hook = NonHook};
         tail = []}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:22], Expected_annotation_sort.FunctionReturn))) |}]

let%expect_test "function_predicates_4" =
  print_sig {|
    function one() { return 1; }
    const n = one()
    export function isOne(x: mixed): boolean %checks {
      return x === n;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"isOne"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [3:16-21];
         name = "isOne"; async = false;
         generator = false; fn_loc = [3:7-48];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [3:25-30]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [3:33-40]));
           predicate = (Some (Predicate ([4:2-17], None)));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "function_predicates_5" =
  print_sig {|
    const one = 1;
    export function isOne(x: mixed): boolean %checks {
      return x === one;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"isOne"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [2:16-21];
         name = "isOne"; async = false;
         generator = false; fn_loc = [2:7-48];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [2:25-30]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:33-40]));
           predicate = (Some (Predicate ([3:2-19], None)));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "function_predicates_6_unsupported_spread" =
  print_sig {|
    export function foo(x: mixed): boolean %checks {
      return foo(...x);
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:7-46];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:23-28]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [1:31-38]));
           predicate = (Some (Predicate ([2:2-19], None)));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "function_predicates_7_latent_with_targs" =
  print_sig {|
    const y = "abc";
    declare export function poly<A, B>(x: A, y: B): boolean;
    export function foo(x: number): boolean %checks {
      return poly<number, _>(x, y);
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 2); (ExportBinding 1)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"; "poly"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "y"; def = (Value (StringLit ([1:10-15], "abc")))}
    1. DeclareFun {id_loc = [2:24-28];
         name = "poly"; fn_loc = [2:28-55];
         def =
         FunSig {
           tparams =
           (Poly ([2:28-34],
              TParam {name_loc = [2:29-30];
                name = "A"; polarity = Polarity.Neutral;
                bound = None; default = None},
              [TParam {name_loc = [2:32-33];
                 name = "B"; polarity = Polarity.Neutral;
                 bound = None; default = None}
                ]
              ));
           params =
           [FunParam {name = (Some "x"); t = (Annot Bound {ref_loc = [2:38-39]; name = "A"})};
             FunParam {name = (Some "y"); t = (Annot Bound {ref_loc = [2:44-45]; name = "B"})}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:48-55]));
           predicate = None; hook = NonHook};
         tail = []}
    2. FunBinding {id_loc = [3:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [3:7-47];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Number [3:23-29]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [3:32-39]));
           predicate =
           (Some (Predicate ([4:2-31],
                    (Some (LatentP ((
                             Ref LocalRef {ref_loc = [4:9-13]; index = 1}),
                             (Some [(ExplicitArg (Annot (Number [4:14-20])));
                                    (ImplicitArg [4:22-23])]),
                             [(Arg
                                 (Ref
                                    BuiltinRef {ref_loc = [4:25-26]; type_ref = false; name = "x"}));
                               (Arg (Ref LocalRef {ref_loc = [4:28-29]; index = 0}))],
                             (("x", 0), []))))
                    )));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "async_function_1" =
  print_sig {|
    async function foo() {};
    module.exports = foo;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [2:17-20]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:15-18];
         name = "foo"; async = true;
         generator = false; fn_loc = [1:6-20];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (AsyncVoidReturn [1:20]);
           predicate = None; hook = NonHook};
         statics = {}}
  |}]

let%expect_test "async_function_2" =
  print_sig {|
    async function foo() { return 1; };
    module.exports = foo;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [2:17-20]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:15-18];
         name = "foo"; async = true;
         generator = false; fn_loc = [1:6-20];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Err [1:20]); predicate = None;
           hook = NonHook};
         statics = {}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:20], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "async_function_3" =
  print_sig {|
    module.exports = async () => await 1;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               FunExpr {loc = [1:17-36];
                 async = true; generator = false;
                 def =
                 FunSig {tparams = Mono;
                   params = []; rest_param = None;
                   this_param = None;
                   return = (Err [1:25]);
                   predicate = None;
                   hook = NonHook};
                 statics = {}}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Errors:
    (SigError
       (Signature_error.ExpectedAnnotation ([1:25], Expected_annotation_sort.FunctionReturn)))
  |}]

let%expect_test "type_spread" =
  print_sig {|
    type A = { a: string };
    type B = { b: number };
    export type C = { ...A, ...B, c: null }
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 2)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"C"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "A";
         tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:9-22];
              obj_kind = InexactObj;
              props =
              { "a" -> (ObjAnnotField ([1:11-12], (Annot (String [1:14-20])), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
    1. TypeAlias {id_loc = [2:5-6]; name = "B";
         tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [2:9-22];
              obj_kind = InexactObj;
              props =
              { "b" -> (ObjAnnotField ([2:11-12], (Annot (Number [2:14-20])), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
    2. TypeAlias {id_loc = [3:12-13];
         name = "C"; tparams = Mono;
         body =
         (Annot
            ObjSpreadAnnot {loc = [3:16-39];
              exact = false;
              elems_rev =
              (ObjSpreadAnnotSlice {dict = None;
                 props =
                 { "c" -> (ObjAnnotField ([3:30-31], (Annot (Null [3:33-37])), Polarity.Neutral)) }},
               [(ObjSpreadAnnotElem (TyRef (Unqualified LocalRef {ref_loc = [3:27-28]; index = 1})));
                 (ObjSpreadAnnotElem
                    (TyRef (Unqualified LocalRef {ref_loc = [3:21-22]; index = 0})))
                 ])})}
  |}]

let%expect_test "inline_interface" =
  print_sig {|
    type A = interface {};
    export type B = interface extends A { p: string };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 1)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"B"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "A";
         tparams = Mono;
         body =
         (Annot
            (InlineInterface ([1:9-21],
               InterfaceSig {extends = []; props = {}; calls = []; dict = None})))}
    1. TypeAlias {id_loc = [2:12-13];
         name = "B"; tparams = Mono;
         body =
         (Annot
            (InlineInterface ([2:16-49],
               InterfaceSig {
                 extends = [(TyRef (Unqualified LocalRef {ref_loc = [2:34-35]; index = 0}))];
                 props =
                 { "p" ->
                   (InterfaceField ((Some [2:38-39]), (
                      Annot (String [2:41-47])), Polarity.Neutral)) };
                 calls = []; dict = None}
               )))}
  |}]

let%expect_test "object_annot_optional" =
  print_sig {|
    export type A = { p?: string };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-30];
              obj_kind = InexactObj;
              props =
              { "p" ->
                (ObjAnnotField ([1:18-19], (
                   Annot (Optional (Annot (String [1:22-28])))), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})} |}]

let%expect_test "interface_optional" =
  print_sig {|
    export interface I { p?: string }
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"I"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. Interface {id_loc = [1:17-18];
         name = "I"; tparams = Mono;
         def =
         InterfaceSig {extends = [];
           props =
           { "p" ->
             (InterfaceField ((Some [1:21-22]), (
                Annot (Optional (Annot (String [1:25-31])))), Polarity.Neutral)) };
           calls = []; dict = None}} |}]

let%expect_test "interface_method" =
  print_sig {|
    export interface I { m(): void }
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"I"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. Interface {id_loc = [1:17-18];
         name = "I"; tparams = Mono;
         def =
         InterfaceSig {extends = [];
           props =
           { "m" ->
             (InterfaceMethod
                (([1:21-22], [1:21-30],
                  FunSig {tparams = Mono;
                    params = []; rest_param = None;
                    this_param = None;
                    return = (Annot (Void [1:26-30]));
                    predicate = None;
                    hook = NonHook}),
                 [])) };
           calls = []; dict = None}}
  |}]

let%expect_test "interface_indexer" =
  print_sig {|
    export interface I {[key: string]: number}
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"I"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. Interface {id_loc = [1:17-18];
         name = "I"; tparams = Mono;
         def =
         InterfaceSig {extends = [];
           props = {}; calls = [];
           dict =
           (Some ObjDict {name = (Some "key");
                   polarity = Polarity.Neutral;
                   key = (Annot (String [1:26-32]));
                   value = (Annot (Number [1:35-41]))})}} |}]

let%expect_test "object_annot_method" =
  print_sig {|
    export type A = { m(): void };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-29];
              obj_kind = InexactObj;
              props =
              { "m" ->
                ObjAnnotMethod {id_loc = [1:18-19];
                  fn_loc = [1:18-27];
                  def =
                  FunSig {tparams = Mono;
                    params = []; rest_param = None;
                    this_param = None;
                    return = (Annot (Void [1:23-27]));
                    predicate = None;
                    hook = NonHook}} };
              proto = ObjAnnotImplicitProto})} |}]

let%expect_test "object_annot_call_poly" =
  print_sig {|
    export type A = { <T>(X): X };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-29];
              obj_kind = InexactObj;
              props = {};
              proto =
              ObjAnnotCallable {
                ts_rev =
                ((Annot
                    (FunAnnot ([1:18-27],
                       FunSig {
                         tparams =
                         (Poly ([1:18-21],
                            TParam {name_loc = [1:19-20];
                              name = "T";
                              polarity = Polarity.Neutral;
                              bound = None;
                              default = None},
                            []));
                         params =
                         [FunParam {name = None;
                            t =
                            (TyRef
                               (Unqualified
                                  BuiltinRef {ref_loc = [1:22-23]; type_ref = true; name = "X"}))}
                           ];
                         rest_param = None;
                         this_param = None;
                         return =
                         (TyRef
                            (Unqualified
                               BuiltinRef {ref_loc = [1:26-27]; type_ref = true; name = "X"}));
                         predicate = None;
                         hook = NonHook}
                       ))),
                 [])}})} |}]

let%expect_test "object_annot_multiple_call" =
  print_sig {|
    export type A = { (): number, (): string };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-42];
              obj_kind = InexactObj;
              props = {};
              proto =
              ObjAnnotCallable {
                ts_rev =
                ((Annot
                    (FunAnnot ([1:30-40],
                       FunSig {tparams = Mono;
                         params = [];
                         rest_param = None;
                         this_param = None;
                         return = (Annot (String [1:34-40]));
                         predicate = None;
                         hook = NonHook}
                       ))),
                 [(Annot
                     (FunAnnot ([1:18-28],
                        FunSig {tparams = Mono;
                          params = [];
                          rest_param = None;
                          this_param = None;
                          return = (Annot (Number [1:22-28]));
                          predicate = None;
                          hook = NonHook}
                        )))
                   ])}})}
  |}]

let%expect_test "destruct_object_shared" =
  print_sig {|
    export const {a, b: {c, d}} = e;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); (ExportBinding 1); (ExportBinding 2)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"; "c"; "d"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:14-15]; name = "a"; def = (Pattern 1)}
    1. Variable {id_loc = [1:21-22]; name = "c"; def = (Pattern 3)}
    2. Variable {id_loc = [1:24-25]; name = "d"; def = (Pattern 4)}

    Pattern defs:
    0. (Ref BuiltinRef {ref_loc = [1:30-31]; type_ref = false; name = "e"})

    Patterns:
    0. (PDef 0)
    1. PropP {id_loc = [1:14-15]; name = "a"; def = 0}
    2. PropP {id_loc = [1:17-18]; name = "b"; def = 0}
    3. PropP {id_loc = [1:21-22]; name = "c"; def = 2}
    4. PropP {id_loc = [1:24-25]; name = "d"; def = 2}
  |}]

let%expect_test "destruct_array_shared" =
  print_sig {|
    export const [a, b, {c, d}] = e;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); (ExportBinding 1); (ExportBinding 2); (ExportBinding 3)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"; "b"; "c"; "d"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:14-15]; name = "a"; def = (Pattern 1)}
    1. Variable {id_loc = [1:17-18]; name = "b"; def = (Pattern 2)}
    2. Variable {id_loc = [1:21-22]; name = "c"; def = (Pattern 4)}
    3. Variable {id_loc = [1:24-25]; name = "d"; def = (Pattern 5)}

    Pattern defs:
    0. (Ref BuiltinRef {ref_loc = [1:30-31]; type_ref = false; name = "e"})

    Patterns:
    0. (PDef 0)
    1. IndexP {loc = [1:14-15]; i = 0; def = 0}
    2. IndexP {loc = [1:17-18]; i = 1; def = 0}
    3. IndexP {loc = [1:20-26]; i = 2; def = 0}
    4. PropP {id_loc = [1:21-22]; name = "c"; def = 3}
    5. PropP {id_loc = [1:24-25]; name = "d"; def = 3}
  |}]

let%expect_test "tuple_annot" =
  print_sig {|
    export type A = [string, number];
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            Tuple {loc = [1:16-32];
              elems_rev =
              [TupleElement {loc = [1:25-31];
                 name = None; t = (Annot (Number [1:25-31]));
                 polarity = Polarity.Neutral;
                 optional = false};
                TupleElement {loc = [1:17-23];
                  name = None; t = (Annot (String [1:17-23]));
                  polarity = Polarity.Neutral;
                  optional = false}
                ]})}
  |}]

let%expect_test "tuple_annot_labeled" =
  print_sig {|
    export type A = [foo: string, bar: number];
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            Tuple {loc = [1:16-42];
              elems_rev =
              [TupleElement {loc = [1:30-41];
                 name = (Some "bar");
                 t = (Annot (Number [1:35-41]));
                 polarity = Polarity.Neutral;
                 optional = false};
                TupleElement {loc = [1:17-28];
                  name = (Some "foo");
                  t = (Annot (String [1:22-28]));
                  polarity = Polarity.Neutral;
                  optional = false}
                ]})}
  |}]

let%expect_test "tuple_annot_variance" =
  print_sig {|
    export type A = [+foo: string, -bar: number];
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            Tuple {loc = [1:16-44];
              elems_rev =
              [TupleElement {loc = [1:31-43];
                 name = (Some "bar");
                 t = (Annot (Number [1:37-43]));
                 polarity = Polarity.Negative;
                 optional = false};
                TupleElement {loc = [1:17-29];
                  name = (Some "foo");
                  t = (Annot (String [1:23-29]));
                  polarity = Polarity.Positive;
                  optional = false}
                ]})}
  |}]

let%expect_test "cycle" =
  print_sig {|
    export type A = { p: ?B };
    export type B = { p: ?A };
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0); (ExportTypeBinding 1)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"A"; "B"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "A"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-25];
              obj_kind = InexactObj;
              props =
              { "p" ->
                (ObjAnnotField ([1:18-19],
                   (Annot
                      (Maybe ([1:21-23],
                         (TyRef (Unqualified LocalRef {ref_loc = [1:22-23]; index = 1}))))),
                   Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
    1. TypeAlias {id_loc = [2:12-13];
         name = "B"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [2:16-25];
              obj_kind = InexactObj;
              props =
              { "p" ->
                (ObjAnnotField ([2:18-19],
                   (Annot
                      (Maybe ([2:21-23],
                         (TyRef (Unqualified LocalRef {ref_loc = [2:22-23]; index = 0}))))),
                   Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
  |}]

let%expect_test "typeof loc" =
  print_sig {|
    export var a: typeof o.p.q;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:11-12];
         name = "a";
         def =
         (Annot
            Typeof {loc = [1:14-26];
              qname = ["q"; "p"; "o"];
              t =
              (Eval ([1:25-26],
                 (Eval ([1:23-24],
                    (Ref BuiltinRef {ref_loc = [1:21-22]; type_ref = false; name = "o"}),
                    (GetProp "p"))),
                 (GetProp "q")));
              targs = None})} |}]

let%expect_test "qualified_generic_typeapp_loc" =
  print_sig {|
    declare export var a: O.P.Q<T>;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-20];
         name = "a";
         def =
         TyRefApp {loc = [1:22-30];
           name =
           Qualified {loc = [1:22-27];
             id_loc = [1:26-27]; name = "Q";
             qualification =
             Qualified {loc = [1:22-25];
               id_loc = [1:24-25]; name = "P";
               qualification =
               (Unqualified BuiltinRef {ref_loc = [1:22-23]; type_ref = true; name = "O"})}};
           targs =
           [(TyRef (Unqualified BuiltinRef {ref_loc = [1:28-29]; type_ref = true; name = "T"}))]}} |}]

let%expect_test "temporary_object_annot" =
  print_sig {|
    declare export var a: $TEMPORARY$object<{foo: string}>;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-20];
         name = "a";
         def =
         (Annot
            (TEMPORARY_Object
               (Annot
                  ObjAnnot {loc = [1:40-53];
                    obj_kind = InexactObj;
                    props =
                    { "foo" ->
                      (ObjAnnotField ([1:41-44], (Annot (String [1:46-52])), Polarity.Neutral)) };
                    proto = ObjAnnotImplicitProto})))} |}]

let%expect_test "export_ref_renaming" =
  print_sig {|
    declare var a: string;
    export {a as b};
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportRef LocalRef {ref_loc = [2:13-14]; index = 0})|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"b"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:12-13]; name = "a"; def = (Annot (String [1:15-21]))} |}]

let%expect_test "union_annot" =
  print_sig {|
    declare export var a: string | number | null;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-20];
         name = "a";
         def =
         (Annot
            Union {loc = [1:22-44]; t0 = (Annot (String [1:22-28]));
              t1 = (Annot (Number [1:31-37]));
              ts = [(Annot (Null [1:40-44]))]})} |}]

let%expect_test "intersection_annot" =
  print_sig {|
    declare export var a: string & number & null;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-20];
         name = "a";
         def =
         (Annot
            Intersection {loc = [1:22-44];
              t0 = (Annot (String [1:22-28]));
              t1 = (Annot (Number [1:31-37]));
              ts = [(Annot (Null [1:40-44]))]})} |}]

let%expect_test "class_extends" =
  print_sig {|
    declare class C {};
    const M = {C};
    export class C1 extends C {};
    export class C2 extends M.C {};
    declare export class C3 extends C {};
    declare export class C4 extends M.C {};
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 2); (ExportBinding 3); (ExportBinding 4); (ExportBinding 5)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"C1"; "C2"; "C3"; "C4"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareClassBinding {id_loc = [1:14-15];
         name = "C";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}}
    1. Variable {id_loc = [2:6-7]; name = "M";
         def =
         (Value
            ObjLit {loc = [2:10-13];
              frozen = false; proto = None;
              props =
              { "C" ->
                (ObjValueField ([2:11-12], (
                   Ref LocalRef {ref_loc = [2:11-12]; index = 0}), Polarity.Neutral)) }})}
    2. ClassBinding {id_loc = [3:13-15];
         name = "C1";
         def =
         ClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [3:24-25];
             t = (Ref LocalRef {ref_loc = [3:24-25]; index = 0})};
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
    3. ClassBinding {id_loc = [4:13-15];
         name = "C2";
         def =
         ClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [4:24-27];
             t = (Eval ([4:24-27], (Ref LocalRef {ref_loc = [4:24-25]; index = 1}), (GetProp "C")))};
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}}
    4. DeclareClassBinding {id_loc = [5:21-23];
         name = "C3";
         def =
         DeclareClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [5:32-33];
             t = (Ref LocalRef {ref_loc = [5:32-33]; index = 0})};
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}}
    5. DeclareClassBinding {id_loc = [6:21-23];
         name = "C4";
         def =
         DeclareClassSig {tparams = Mono;
           extends =
           ClassExplicitExtends {loc = [6:32-35];
             t = (Eval ([6:32-35], (Ref LocalRef {ref_loc = [6:32-33]; index = 1}), (GetProp "C")))};
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}} |}]

let%expect_test "class_this" =
  print_sig {|
    export class C {
      m(): this { return this };
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"C"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:13-14];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "m" ->
             ObjValueMethod {id_loc = [2:2-3];
               fn_loc = [2:2-27]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = []; rest_param = None;
                 this_param = None; return = (Annot Bound {ref_loc = [2:7-11]; name = "this"});
                 predicate = None; hook = NonHook}} };
           own_props = {}}} |}]

let%expect_test "declare_class_this" =
  print_sig {|
    declare export class C {
      m(): this;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"C"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareClassBinding {id_loc = [1:21-22];
         name = "C";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props =
           { "m" ->
             (InterfaceMethod
                (([2:2-3], [2:2-11],
                  FunSig {tparams = Mono;
                    params = []; rest_param = None;
                    this_param = None;
                    return = (Annot Bound {ref_loc = [2:7-11]; name = "this"});
                    predicate = None;
                    hook = NonHook}),
                 [])) };
           static_calls = []; calls = [];
           dict = None; static_dict = None}} |}]

let%expect_test "existential" =
  print_sig {|
    class C<T> {
      p: *;
    };
    declare export default C<*>;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [4:15-22];
          def =
          TyRefApp {loc = [4:23-27];
            name = (Unqualified LocalRef {ref_loc = [4:23-24]; index = 0});
            targs = [(Annot (Exists [4:25-26]))]}}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {
           tparams =
           (Poly ([1:7-10],
              TParam {name_loc = [1:8-9];
                name = "T"; polarity = Polarity.Neutral;
                bound = None; default = None},
              []));
           extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {};
           own_props =
           { "p" -> (ObjValueField ([2:2-3], (Annot (Exists [2:5-6])), Polarity.Neutral)) }}} |}]

let%expect_test "exact_by_default" =
  print_sig ~exact_by_default:true {|
    export type T = { p: string }
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "T"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-29];
              obj_kind = ExactObj;
              props =
              { "p" -> (ObjAnnotField ([1:18-19], (Annot (String [1:21-27])), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})} |}]

let%expect_test "cjs_export_props" =
  print_sig {|
    module.exports.foo = 0;
    exports.bar = 1;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [0:0]; frozen = true;
                 proto = None;
                 props =
                 { "bar" ->
                   (ObjValueField ([2:8-11], (
                      Value (NumberLit ([2:14-15], 1., "1"))), Polarity.Neutral));
                   "foo" ->
                   (ObjValueField ([1:15-18], (
                      Value (NumberLit ([1:21-22], 0., "0"))), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "cjs_exports_clobber_shadowed_module_global" =
  print_sig {|
    var module;
    module.exports = 0;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "cjs_exports_assign_shadowed_exports_global" =
  print_sig {|
    var exports;
    exports.foo = 0;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "cjs_exports_assign_shadowed_module_global" =
  print_sig {|
    var module;
    module.exports.foo = 0;
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "cjs_export_shadowed_hoisted_TODO" =
  print_sig {|
    module.exports.foo = 0;
    function module() {}
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [0:0]; frozen = true;
                 proto = None;
                 props =
                 { "foo" ->
                   (ObjValueField ([1:15-18], (
                      Value (NumberLit ([1:21-22], 0., "0"))), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}
  |}]

let%expect_test "cjs_export_fun_expr_props" =
  print_sig {|
    module.exports = function() {}
    module.exports.foo = 0;
    exports.bar = 1;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               FunExpr {loc = [1:17-27];
                 async = false; generator = false;
                 def =
                 FunSig {tparams = Mono;
                   params = []; rest_param = None;
                   this_param = None;
                   return = (Annot (Void [1:27]));
                   predicate = None;
                   hook = NonHook};
                 statics =
                 { "bar" -> ([3:8-11], (Value (NumberLit ([3:14-15], 1., "1"))));
                   "foo" -> ([2:15-18], (Value (NumberLit ([2:21-22], 0., "0")))) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "cjs_export_fun_binding_props" =
  print_sig {|
    module.exports = function foo() {}
    module.exports.foo = 0;
    exports.bar = 1;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [1:26-29]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:26-29];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:17-31];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:31]));
           predicate = None; hook = NonHook};
         statics =
         { "bar" -> ([3:8-11], (Value (NumberLit ([3:14-15], 1., "1"))));
           "foo" -> ([2:15-18], (Value (NumberLit ([2:21-22], 0., "0")))) }} |}]

let%expect_test "es_export_named_fun_props" =
  print_sig {|
    export function foo() {}
    foo.bar = 1;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:7-21];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:21]));
           predicate = None; hook = NonHook};
         statics = { "bar" -> ([2:4-7], (Value (NumberLit ([2:10-11], 1., "1")))) }} |}]

let%expect_test "es_export_default_fun_props" =
  print_sig {|
    export default function foo() {}
    foo.bar = 1;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:7-14]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:24-27];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:15-29];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:29]));
           predicate = None; hook = NonHook};
         statics = { "bar" -> ([2:4-7], (Value (NumberLit ([2:10-11], 1., "1")))) }} |}]

let%expect_test "fun_binding_assign" =
  print_sig {|
    function foo() {}
    foo.bar = 0;
    module.exports = foo;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [3:17-20]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-12];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:0-14];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:14]));
           predicate = None; hook = NonHook};
         statics = { "bar" -> ([2:4-7], (Value (NumberLit ([2:10-11], 0., "0")))) }} |}]

let%expect_test "fun_const_assign" =
  print_sig {|
    const foo = function() {};
    foo.bar = 0;
    module.exports = foo;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [3:17-20]; index = 0}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:6-9]; name = "foo";
         def =
         (Value
            FunExpr {loc = [1:12-22];
              async = false; generator = false;
              def =
              FunSig {tparams = Mono;
                params = []; rest_param = None;
                this_param = None; return = (Annot (Void [1:22]));
                predicate = None; hook = NonHook};
              statics = { "bar" -> ([2:4-7], (Value (NumberLit ([2:10-11], 0., "0")))) }})} |}]

let%expect_test "ref_const_assign" =
  print_sig {|
    const foo = function f() {};
    foo.bar = 0;
    module.exports = foo;
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some (Ref LocalRef {ref_loc = [3:17-20]; index = 1}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:21-22];
         name = "f"; async = false; generator = false;
         fn_loc = [1:12-24];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:24]));
           predicate = None; hook = NonHook};
         statics = { "bar" -> ([2:4-7], (Value (NumberLit ([2:10-11], 0., "0")))) }}
    1. Variable {id_loc = [1:6-9]; name = "foo";
         def = (Ref LocalRef {ref_loc = [1:21-22]; index = 0})} |}]

let%expect_test "obj_annot_proto" =
  print_sig {|
    declare export var o: { __proto__: null };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"o"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-20];
         name = "o";
         def =
         (Annot
            ObjAnnot {loc = [1:22-41];
              obj_kind = InexactObj;
              props = {}; proto = (ObjAnnotExplicitProto ([1:35-39], (Annot (Null [1:35-39]))))})} |}]

let%expect_test "getter_setter" =
  print_sig {|
    export const a = { get p(): number { return 0 } };
    export const b = { set p(x: number): void {} };
    export const c = { get p(): number { return 0 }, set p(x: number): void {} };
    export const d = { get p(): number { return 0 }, get p(): string { return "" } };
    export const e = { set p(x: number): void {}, set p(x: string): void {} };
    export const f = {
      get p(): number { return 0 },
      set p(x: number): void {},
      get p(): string { return "" },
    };
    export const g = {
      get p(): number { return 0 },
      set p(x: number): void {},
      set p(x: string): void {},
    };
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|(ExportBinding 0); (ExportBinding 1); (
        ExportBinding 2); (ExportBinding 3); (
        ExportBinding 4); (ExportBinding 5); (
        ExportBinding 6)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"; "b"; "c"; "d"; "e"; "f"; "g"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:13-14];
         name = "a";
         def =
         (Value
            ObjLit {loc = [1:17-49];
              frozen = false; proto = None;
              props = { "p" -> (ObjValueAccess (Get ([1:23-24], (Annot (Number [1:28-34]))))) }})}
    1. Variable {id_loc = [2:13-14];
         name = "b";
         def =
         (Value
            ObjLit {loc = [2:17-46];
              frozen = false; proto = None;
              props = { "p" -> (ObjValueAccess (Set ([2:23-24], (Annot (Number [2:28-34]))))) }})}
    2. Variable {id_loc = [3:13-14];
         name = "c";
         def =
         (Value
            ObjLit {loc = [3:17-76];
              frozen = false; proto = None;
              props =
              { "p" ->
                (ObjValueAccess
                   (GetSet ([3:23-24], (
                      Annot (Number [3:28-34])), [3:53-54], (
                      Annot (Number [3:58-64]))))) }})}
    3. Variable {id_loc = [4:13-14];
         name = "d";
         def =
         (Value
            ObjLit {loc = [4:17-80];
              frozen = false; proto = None;
              props = { "p" -> (ObjValueAccess (Get ([4:53-54], (Annot (String [4:58-64]))))) }})}
    4. Variable {id_loc = [5:13-14];
         name = "e";
         def =
         (Value
            ObjLit {loc = [5:17-73];
              frozen = false; proto = None;
              props = { "p" -> (ObjValueAccess (Set ([5:50-51], (Annot (String [5:55-61]))))) }})}
    5. Variable {id_loc = [6:13-14];
         name = "f";
         def =
         (Value
            ObjLit {loc = [6:17-10:1];
              frozen = false; proto = None;
              props =
              { "p" ->
                (ObjValueAccess
                   (GetSet ([9:6-7], (
                      Annot (String [9:11-17])), [8:6-7], (
                      Annot (Number [8:11-17]))))) }})}
    6. Variable {id_loc = [11:13-14];
         name = "g";
         def =
         (Value
            ObjLit {loc = [11:17-15:1];
              frozen = false; proto = None;
              props =
              { "p" ->
                (ObjValueAccess
                   (GetSet ([12:6-7], (
                      Annot (Number [12:11-17])), [14:6-7], (
                      Annot (String [14:11-17]))))) }})}
  |}]

let%expect_test "predicate_exists" =
  print_sig {|
    export default function(x: ?string): mixed %checks {
      return x;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "x");
                    t = (Annot (Maybe ([1:27-34], (Annot (String [1:28-34])))))}
                   ];
                 rest_param = None; this_param = None;
                 return = (Annot (Mixed [1:37-42]));
                 predicate = (Some (Predicate ([2:2-11], (Some (ExistsP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_instanceof" =
  print_sig {|
    class C {}
    export default function(x: mixed): boolean %checks {
      return x instanceof C;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             FunExpr {loc = [2:15-4:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [2:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [2:35-42]));
                 predicate =
                 (Some (Predicate ([3:2-24],
                          (Some (InstanceofP ("x", (
                                   Ref LocalRef {ref_loc = [3:22-23]; index = 0}))))
                          )));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:6-7];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}} |}]

let%expect_test "predicate_typeof" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return typeof x == "string";
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-30], (Some (StrP ("x", [2:9-29]))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_typeof_template_literal" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return typeof x == `string`;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-30], (Some (StrP ("x", [2:9-29]))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_literal_string" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === "foo";
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate =
                 (Some (Predicate ([2:2-21], (Some (SingletonStrP ("x", [2:15-20], true, "foo"))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_literal_number" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === 42;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate =
                 (Some (Predicate ([2:2-18],
                          (Some (SingletonNumP ("x", [2:15-17], true, 42., "42"))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_literal_boolean" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === true;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate =
                 (Some (Predicate ([2:2-20], (Some (SingletonBoolP ("x", [2:15-19], true))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_literal_null" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === null;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-20], (Some (NullP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_maybe" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x == null;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-19], (Some (MaybeP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_template_literal" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === `foo`;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate =
                 (Some (Predicate ([2:2-21], (Some (SingletonStrP ("x", [2:15-20], true, "foo"))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_negative_number_literal" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === -42;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate =
                 (Some (Predicate ([2:2-19],
                          (Some (SingletonNumP ("x", [2:15-18], true, -42., "-42"))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_undefined" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === undefined;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-25], (Some (VoidP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_undefined_shadow" =
  (* TODO: should not extract predicate *)
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === undefined;
    }
    var undefined = 42;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-25], (Some (VoidP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_void" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return x === void 0;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-22], (Some (VoidP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_sentinel_string" =
  print_sig {|
    import type {T} from "foo";
    export default function(o: T): boolean %checks {
      return o.type === "foo";
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [2:7-14];
          def =
          (Value
             FunExpr {loc = [2:15-4:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "o");
                    t = (TyRef (Unqualified RemoteRef {ref_loc = [2:27-28]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [2:31-38]));
                 predicate =
                 (Some (Predicate ([3:2-26], (Some (
                          SentinelStrP ("o", "type", [3:20-25], "foo"))))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. foo

    Remote refs:
    0. ImportType {id_loc = [1:13-14]; name = "T"; index = 0; remote = "T"} |}]

let%expect_test "predicate_sentinel_expr" =
  print_sig {|
    import type {T} from "foo";
    const foo = "foo";
    export default function(o: T): boolean %checks {
      return o.type === foo;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [3:7-14];
          def =
          (Value
             FunExpr {loc = [3:15-5:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params =
                 [FunParam {name = (Some "o");
                    t = (TyRef (Unqualified RemoteRef {ref_loc = [3:27-28]; index = 0}))}
                   ];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [3:31-38]));
                 predicate =
                 (Some (Predicate ([4:2-24],
                          (Some (SentinelExprP ("o", "type",
                                   (Ref LocalRef {ref_loc = [4:20-23]; index = 0}))))
                          )));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Module refs:
    0. foo

    Local defs:
    0. Variable {id_loc = [2:6-9]; name = "foo"; def = (Value (StringLit ([2:12-17], "foo")))}

    Remote refs:
    0. ImportType {id_loc = [1:13-14]; name = "T"; index = 0; remote = "T"} |}]

let%expect_test "predicate_Array_isArray" =
  print_sig {|
    export default function(x: mixed): boolean %checks {
      return Array.isArray(x);
    }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports =
      [|ExportDefault {default_loc = [1:7-14];
          def =
          (Value
             FunExpr {loc = [1:15-3:1];
               async = false; generator = false;
               def =
               FunSig {tparams = Mono;
                 params = [FunParam {name = (Some "x"); t = (Annot (Mixed [1:27-32]))}];
                 rest_param = None; this_param = None;
                 return = (Annot (Boolean [1:35-42]));
                 predicate = (Some (Predicate ([2:2-26], (Some (ArrP "x")))));
                 hook = NonHook};
               statics = {}})}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}} |}]

let%expect_test "predicate_latent" =
  print_sig {|
    function f(a: mixed): boolean %checks {
      return typeof a === "string";
    }
    function g(_: mixed, b: mixed): boolean %checks {
      return typeof b === "number";
    }
    export function h(a: mixed, b: mixed): boolean %checks {
      return (f(a) && g(a, b));
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 2)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"h"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:9-10];
         name = "f"; async = false; generator = false;
         fn_loc = [1:0-37];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "a"); t = (Annot (Mixed [1:14-19]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [1:22-29]));
           predicate = (Some (Predicate ([2:2-31], (Some (StrP ("a", [2:9-30]))))));
           hook = NonHook};
         statics = {}}
    1. FunBinding {id_loc = [4:9-10];
         name = "g"; async = false; generator = false;
         fn_loc = [4:0-47];
         def =
         FunSig {tparams = Mono;
           params =
           [FunParam {name = (Some "_"); t = (Annot (Mixed [4:14-19]))};
             FunParam {name = (Some "b"); t = (Annot (Mixed [4:24-29]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [4:32-39]));
           predicate = (Some (Predicate ([5:2-31], (Some (NumP ("b", [5:9-30]))))));
           hook = NonHook};
         statics = {}}
    2. FunBinding {id_loc = [7:16-17];
         name = "h"; async = false; generator = false;
         fn_loc = [7:7-54];
         def =
         FunSig {tparams = Mono;
           params =
           [FunParam {name = (Some "a"); t = (Annot (Mixed [7:21-26]))};
             FunParam {name = (Some "b"); t = (Annot (Mixed [7:31-36]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [7:39-46]));
           predicate =
           (Some (Predicate ([8:2-27],
                    (Some (AndP (
                             (LatentP ((
                                Ref LocalRef {ref_loc = [8:10-11]; index = 0}), None,
                                [(Arg
                                    (
                                    Ref
                                    BuiltinRef {ref_loc = [8:12-13]; type_ref = false; name = "a"}))
                                  ],
                                (("a", 0), []))),
                             (LatentP ((
                                Ref LocalRef {ref_loc = [8:18-19]; index = 1}), None,
                                [(Arg
                                    (
                                    Ref
                                    BuiltinRef {ref_loc = [8:20-21]; type_ref = false; name = "a"}));
                                  (Arg
                                    (Ref
                                    BuiltinRef {ref_loc = [8:23-24]; type_ref = false; name = "b"}))
                                  ],
                                (("b", 1), [("a", 0)])))
                             )))
                    )));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "predicate_latent_non_param_identifiers" =
  print_sig {|
    const y = "abc";
    declare function foo(x: mixed, y: mixed): boolean %checks(typeof x === "number");
    export function bar(x: mixed): boolean %checks {
      return foo(x, y);
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 2)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"bar"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "y"; def = (Value (StringLit ([1:10-15], "abc")))}
    1. DeclareFun {id_loc = [2:17-20];
         name = "foo"; fn_loc = [2:20-49];
         def =
         FunSig {tparams = Mono;
           params =
           [FunParam {name = (Some "x"); t = (Annot (Mixed [2:24-29]))};
             FunParam {name = (Some "y"); t = (Annot (Mixed [2:34-39]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [2:42-49]));
           predicate = (Some (Predicate ([2:50-80], (Some (NumP ("x", [2:58-79]))))));
           hook = NonHook};
         tail = []}
    2. FunBinding {id_loc = [3:16-19];
         name = "bar"; async = false;
         generator = false; fn_loc = [3:7-46];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (Mixed [3:23-28]))}];
           rest_param = None; this_param = None;
           return = (Annot (Boolean [3:31-38]));
           predicate =
           (Some (Predicate ([4:2-19],
                    (Some (LatentP ((
                             Ref LocalRef {ref_loc = [4:9-12]; index = 1}), None,
                             [(Arg
                                 (Ref
                                    BuiltinRef {ref_loc = [4:13-14]; type_ref = false; name = "x"}));
                               (Arg (Ref LocalRef {ref_loc = [4:16-17]; index = 0}))],
                             (("x", 0), []))))
                    )));
           hook = NonHook};
         statics = {}} |}]

let%expect_test "long_string_lit" =
  print_sig ~max_literal_len:3 {|
    export const a = "aaa";
    export const b = "bbbb";
    declare export var c: $TEMPORARY$string<"ccc">;
    declare export var d: $TEMPORARY$string<"dddd">;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); (ExportBinding 1); (ExportBinding 2); (ExportBinding 3)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"a"; "b"; "c"; "d"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:13-14]; name = "a"; def = (Value (StringLit ([1:17-22], "aaa")))}
    1. Variable {id_loc = [2:13-14]; name = "b"; def = (Value (LongStringLit [2:17-23]))}
    2. Variable {id_loc = [3:19-20];
         name = "c"; def = (Annot (TEMPORARY_String ([3:40-45], "ccc")))}
    3. Variable {id_loc = [4:19-20]; name = "d"; def = (Annot (TEMPORARY_LongString [4:40-46]))} |}]

let%expect_test "export_default_function_binding" =
  print_sig {|
    export default function f(): void {}
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:7-14]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:24-25];
         name = "f"; async = false; generator = false;
         fn_loc = [1:15-33];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:29-33]));
           predicate = None; hook = NonHook};
         statics = {}} |}]

let%expect_test "export_default_class_binding" =
  print_sig {|
    export default class C {}
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:7-14]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:21-22];
         name = "C";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props = {}; own_props = {}}} |}]

let%expect_test "declared_export_default_function_binding" =
  print_sig {|
    declare export default function f(): void;
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:15-22]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareFun {id_loc = [1:32-33];
         name = "f"; fn_loc = [1:33-41];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [1:37-41]));
           predicate = None; hook = NonHook};
         tail = []} |}]

let%expect_test "declared_export_default_class_binding" =
  print_sig {|
    declare export default class C {};
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:15-22]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareClassBinding {id_loc = [1:29-30];
         name = "C";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}} |}]

let%expect_test "module_ref_prefix" =
  print_sig ~module_ref_prefix:"m#" {|
    module.exports = "m#foo";
  |};
  [%expect {|
    CJSModule {type_exports = [||];
      exports = (Some ModuleRef {loc = [1:17-24]; index = 0; legacy_interop = false});
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Module refs:
    0. foo |}]

let%expect_test "enum_export" =
  print_sig {|
    export enum E { A, B };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = StringRep {truthy = true};
         members = { "A" -> [1:16-17]; "B" -> [1:19-20] };
         has_unknown_members = false} |}]

let%expect_test "enum_default_export" =
  print_sig {|
    export default enum E { A, B }
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|ExportDefaultBinding {default_loc = [1:7-14]; index = 0}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:20-21];
         name = "E"; rep = StringRep {truthy = true};
         members = { "A" -> [1:24-25]; "B" -> [1:27-28] };
         has_unknown_members = false}
  |}]

let%expect_test "enum_stmt" =
  print_sig {|
    enum E { A, B };
    export {E}
  |};
  [%expect {|
    ESModule {type_exports = [||];
      exports = [|(ExportRef LocalRef {ref_loc = [2:8-9]; index = 0})|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:5-6];
         name = "E"; rep = StringRep {truthy = true};
         members = { "A" -> [1:9-10]; "B" -> [1:12-13] };
         has_unknown_members = false} |}]

let%expect_test "enum_bool_lit" =
  print_sig {|
    export enum E { A = true }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = (BoolRep (Some true));
         members = { "A" -> [1:16-24] };
         has_unknown_members = false} |}]

let%expect_test "enum_bool" =
  print_sig {|
    export enum E { A = true, B = false }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = (BoolRep None);
         members = { "A" -> [1:16-24]; "B" -> [1:26-35] };
         has_unknown_members = false} |}]

let%expect_test "enum_number_truthy" =
  print_sig {|
    export enum E { A = 1, B = 2 }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = NumberRep {truthy = true};
         members = { "A" -> [1:16-21]; "B" -> [1:23-28] };
         has_unknown_members = false} |}]

let%expect_test "enum_number_any" =
  print_sig {|
    export enum E { A = 0, B = 1 }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = NumberRep {truthy = false};
         members = { "A" -> [1:16-21]; "B" -> [1:23-28] };
         has_unknown_members = false} |}]

let%expect_test "enum_string_any" =
  print_sig {|
    export enum E { A = "", B = "B" }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = StringRep {truthy = false};
         members = { "A" -> [1:16-22]; "B" -> [1:24-31] };
         has_unknown_members = false} |}]

let%expect_test "enum_symbol" =
  print_sig {|
    export enum E of symbol { A, B }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = SymbolRep;
         members = { "A" -> [1:26-27]; "B" -> [1:29-30] };
         has_unknown_members = false} |}]

let%expect_test "enum_unknown_members" =
  print_sig {|
    export enum E { A, B, ... };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:12-13];
         name = "E"; rep = StringRep {truthy = true};
         members = { "A" -> [1:16-17]; "B" -> [1:19-20] };
         has_unknown_members = true} |}]

let%expect_test "enum_declared" =
  print_sig {|
    declare export enum E { A, B };
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. EnumBinding {id_loc = [1:20-21];
         name = "E"; rep = StringRep {truthy = true};
         members = { "A" -> [1:24-25]; "B" -> [1:27-28] };
         has_unknown_members = false} |}]

let%expect_test "enum_disabled" =
  print_sig ~enable_enums:false {|
    export enum E {}
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"E"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DisabledEnumBinding {id_loc = [1:12-13]; name = "E"} |}]

let%expect_test "builtins" =
  print_builtins [{|
    declare var x: T;
    type T = string;
  |}];
  [%expect {|
    Local defs:
    0. Variable {id_loc = [1:12-13];
         name = "x"; def = (TyRef (Unqualified LocalRef {ref_loc = [1:15-16]; index = 1}))}
    1. TypeAlias {id_loc = [2:5-6]; name = "T"; tparams = Mono; body = (Annot (String [2:9-15]))}

    Builtin global value x
    Builtin global type T |}]

let%expect_test "builtins_ignore_name_def_for_use_special_cased_names" =
  print_builtins [{|
    type T1 = Array<string>;
    type T2 = $ReadOnly<{foo: bar}>;
    declare class Array {}
    type $ReadOnly = number;
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [1:5-7]; name = "T1";
         tparams = Mono; body = (Annot (Array ([1:10-23], (Annot (String [1:16-22])))))}
    1. TypeAlias {id_loc = [2:5-7]; name = "T2";
         tparams = Mono;
         body =
         (Annot
            (ReadOnly ([2:10-31],
               (Annot
                  ObjAnnot {loc = [2:20-30];
                    obj_kind = InexactObj;
                    props =
                    { "foo" ->
                      (ObjAnnotField ([2:21-24],
                         (TyRef
                            (Unqualified
                               BuiltinRef {ref_loc = [2:26-29]; type_ref = true; name = "bar"})),
                         Polarity.Neutral)) };
                    proto = ObjAnnotImplicitProto})
               )))}
    2. DeclareClassBinding {id_loc = [3:14-19];
         name = "Array";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}}
    3. TypeAlias {id_loc = [4:5-14];
         name = "$ReadOnly"; tparams = Mono;
         body = (Annot (Number [4:17-23]))}

    Builtin global value Array
    Builtin global type $ReadOnly
    Builtin global type T1
    Builtin global type T2 |}]

let%expect_test "builtin_cjs_module" =
  print_builtins [{|
    type T = string;
    declare module foo {
      declare module.exports: T;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "T"; tparams = Mono; body = (Annot (String [1:9-15]))}

    Builtin global type T
    Builtin module foo:
    [2:15-18] CJSModule {type_exports = [||];
                exports = (Some (TyRef (Unqualified LocalRef {ref_loc = [3:26-27]; index = 0})));
                info =
                CJSModuleInfo {type_export_keys = [||];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_ignore_later" =
  print_builtins [{|
    type T = string;
    declare module foo {
      declare module.exports: string;
    }
    declare module foo {
      declare module.exports: number;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "T"; tparams = Mono; body = (Annot (String [1:9-15]))}

    Builtin global type T
    Builtin module foo:
    [2:15-18] CJSModule {type_exports = [||];
                exports = (Some (Annot (String [3:26-32])));
                info =
                CJSModuleInfo {type_export_keys = [||];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_module_auto_export_type" =
  (* All types in cjs modules are auto exported. *)
  print_builtins [{|
    declare module foo {
      declare type T = number;
      declare module.exports: string;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [2:15-16]; name = "T"; tparams = Mono; body = (Annot (Number [2:19-25]))}

    Builtin module foo:
    [1:15-18] CJSModule {type_exports = [|(ExportTypeBinding 0)|];
                exports = (Some (Annot (String [3:26-32])));
                info =
                CJSModuleInfo {type_export_keys = [|"T"|];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_module_unused_type_exported" =
  (* T is exported because it has an `export` keyword *)
  print_builtins [{|
    declare module foo {
      declare export type T = number;
      declare module.exports: string;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [2:22-23]; name = "T"; tparams = Mono; body = (Annot (Number [2:26-32]))}

    Builtin module foo:
    [1:15-18] CJSModule {type_exports = [|(ExportTypeBinding 0)|];
                exports = (Some (Annot (String [3:26-32])));
                info =
                CJSModuleInfo {type_export_keys = [|"T"|];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_module_used_type" =
  (* T is included because it is reachable, but not a named export because it does not
     have an `export` keyword *)
  print_builtins [{|
    declare module foo {
      declare type T = number;
      declare module.exports: T;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [2:15-16]; name = "T"; tparams = Mono; body = (Annot (Number [2:19-25]))}

    Builtin module foo:
    [1:15-18] CJSModule {type_exports = [|(ExportTypeBinding 0)|];
                exports = (Some (TyRef (Unqualified LocalRef {ref_loc = [3:26-27]; index = 0})));
                info =
                CJSModuleInfo {type_export_keys = [|"T"|];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_module_used_type_exported" =
  (* `T` is exported because it has an `export` keyword, and is also reachable via the default export *)
  print_builtins [{|
    declare module foo {
      declare export type T = number;
      declare module.exports: T;
    }
  |}];
  [%expect {|
    Local defs:
    0. TypeAlias {id_loc = [2:22-23]; name = "T"; tparams = Mono; body = (Annot (Number [2:26-32]))}

    Builtin module foo:
    [1:15-18] CJSModule {type_exports = [|(ExportTypeBinding 0)|];
                exports = (Some (TyRef (Unqualified LocalRef {ref_loc = [3:26-27]; index = 0})));
                info =
                CJSModuleInfo {type_export_keys = [|"T"|];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_cjs_module_with_implicit_exports" =
  (* when a `declare module` doesn't have explicit value exports via `declare export` (which
     makes it an ES module) or `declare module.exports = ...` (which makes it a CJS module),
     it is implicitly a CJS module where all values are exported.

     types are always exported in CJS modules, and require the `export` keyword in ES modules.
     NOTE: `declare export type` does not an ES module make!

     so in this test:
      - there are no explicitly exported values, so everything is implicitly exported
      - explicitly exporting type U does not hide T; T is also exported *)
  print_builtins [{|
    declare module foo {
      declare var x: string;
      declare const y: string;
      declare let z: string;
      const zRef = z;
      function empty() {}
      declare function f(): void;
      declare class Y {}
      declare component foo()
      declare enum A { B }
      declare type T = number;
      declare export type U = string;
    }
  |}];
  [%expect {|
    Local defs:
    0. Variable {id_loc = [2:14-15]; name = "x"; def = (Annot (String [2:17-23]))}
    1. Variable {id_loc = [3:16-17]; name = "y"; def = (Annot (String [3:19-25]))}
    2. Variable {id_loc = [4:14-15]; name = "z"; def = (Annot (String [4:17-23]))}
    3. DeclareFun {id_loc = [7:19-20];
         name = "f"; fn_loc = [7:20-28];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (Void [7:24-28]));
           predicate = None; hook = NonHook};
         tail = []}
    4. DeclareClassBinding {id_loc = [8:16-17];
         name = "Y";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props = {}; static_calls = [];
           calls = []; dict = None; static_dict = None}}
    5. ComponentBinding {id_loc = [9:20-23];
         name = "foo"; fn_loc = [9:2-25];
         def =
         ComponentSig {params_loc = [9:23-25];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([9:25],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [9:25]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}
    6. EnumBinding {id_loc = [10:15-16];
         name = "A"; rep = StringRep {truthy = true};
         members = { "B" -> [10:19-20] };
         has_unknown_members = false}
    7. TypeAlias {id_loc = [11:15-16];
         name = "T"; tparams = Mono;
         body = (Annot (Number [11:19-25]))}
    8. TypeAlias {id_loc = [12:22-23];
         name = "U"; tparams = Mono;
         body = (Annot (String [12:26-32]))}

    Builtin module foo:
    [1:15-18] CJSModule {type_exports = [|(ExportTypeBinding 7); (ExportTypeBinding 8)|];
                exports =
                (Some (Value
                         DeclareModuleImplicitlyExportedObject {
                           loc = [1:15-18];
                           module_name = "foo";
                           props =
                           { "A" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 6}), Polarity.Positive
                                ));
                             "Y" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 4}), Polarity.Positive
                                ));
                             "f" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 3}), Polarity.Positive
                                ));
                             "foo" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 5}), Polarity.Positive
                                ));
                             "x" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 0}), Polarity.Positive
                                ));
                             "y" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 1}), Polarity.Positive
                                ));
                             "z" ->
                             (ObjValueField ([1:15-18],
                                (Ref LocalRef {ref_loc = [1:15-18]; index = 2}), Polarity.Positive
                                )) }}));
                info =
                CJSModuleInfo {type_export_keys = [|"T"; "U"|];
                  type_stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_es_module_default" =
  print_builtins [{|
    declare module foo {
      declare export default string;
    }
  |}];
  [%expect {|
    Builtin module foo:
    [1:15-18] ESModule {type_exports = [||];
                exports =
                [|ExportDefault {default_loc = [2:17-24]; def = (Annot (String [2:25-31]))}|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"default"|];
                  stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_module_import_typeof" =
  print_builtins [{|
    declare module foo {
      declare export var x: string;
    }
    declare module bar {
      import typeof {x} from 'foo';
      declare export var y: x;
    }
  |}];
  [%expect {|
    Module refs:
    0. foo

    Local defs:
    0. Variable {id_loc = [2:21-22]; name = "x"; def = (Annot (String [2:24-30]))}
    1. Variable {id_loc = [6:21-22];
         name = "y"; def = (TyRef (Unqualified RemoteRef {ref_loc = [6:24-25]; index = 0}))}

    Remote refs:
    0. ImportTypeof {id_loc = [5:17-18]; name = "x"; index = 0; remote = "x"}

    Builtin module bar:
    [4:15-18] ESModule {type_exports = [||];
                exports = [|(ExportBinding 1)|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"y"|];
                  stars = []; strict = true;
                  platform_availability_set = None}}
    Builtin module foo:
    [1:15-18] ESModule {type_exports = [||];
                exports = [|(ExportBinding 0)|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"x"|];
                  stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_toplevel_import" =
  (* this should be a parse error, but in the meantime, make sure we don't fatal.
     the `import` gets ignored and the `x` becomes a BuiltinRef. *)
  print_builtins [{|
    declare module foo {
      declare export var x: string;
    }
    import typeof {x} from 'foo';
    declare module bar {
      declare export var y: x;
    }
  |}];
  [%expect{|
    Local defs:
    0. Variable {id_loc = [2:21-22]; name = "x"; def = (Annot (String [2:24-30]))}
    1. Variable {id_loc = [6:21-22];
         name = "y";
         def = (TyRef (Unqualified BuiltinRef {ref_loc = [6:24-25]; type_ref = true; name = "x"}))}

    Builtin module bar:
    [5:15-18] ESModule {type_exports = [||];
                exports = [|(ExportBinding 1)|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"y"|];
                  stars = []; strict = true;
                  platform_availability_set = None}}
    Builtin module foo:
    [1:15-18] ESModule {type_exports = [||];
                exports = [|(ExportBinding 0)|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"x"|];
                  stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_module_export_specifiers" =
  print_builtins [{|
    declare module "foo" {
      declare var x : string;
      declare var y : string;
      declare export {x, y};
    }
  |}];
  [%expect {|
    Local defs:
    0. Variable {id_loc = [2:14-15]; name = "x"; def = (Annot (String [2:18-24]))}
    1. Variable {id_loc = [3:14-15]; name = "y"; def = (Annot (String [3:18-24]))}

    Builtin module foo:
    [1:15-20] ESModule {type_exports = [||];
                exports =
                [|(ExportRef LocalRef {ref_loc = [4:18-19]; index = 0});
                  (ExportRef LocalRef {ref_loc = [4:21-22]; index = 1})|];
                info =
                ESModuleInfo {type_export_keys = [||];
                  type_stars = []; export_keys = [|"x"; "y"|];
                  stars = []; strict = true;
                  platform_availability_set = None}} |}]

let%expect_test "builtin_declare_namespace" =
  print_builtins [{|
    declare namespace ns {
      declare export const bar1: number;
      declare const bar2: boolean;
      declare var bar3: boolean;
      declare function f(): string;
      declare function f(): number;
      declare type Baz = string;
      enum B {
        C,
        D,
      }
      if (true) {} // unsupported
      declare module.exports: {foo: string}; // unsupported
      import React from 'react'; // unsupported
    }
  |}];
  [%expect {|
    Local defs:
    0. Variable {id_loc = [2:23-27]; name = "bar1"; def = (Annot (Number [2:29-35]))}
    1. Variable {id_loc = [3:16-20]; name = "bar2"; def = (Annot (Boolean [3:22-29]))}
    2. Variable {id_loc = [4:14-18]; name = "bar3"; def = (Annot (Boolean [4:20-27]))}
    3. DeclareFun {id_loc = [5:19-20];
         name = "f"; fn_loc = [5:20-30];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = None;
           return = (Annot (String [5:24-30]));
           predicate = None; hook = NonHook};
         tail =
         [([6:19-20], [6:20-30],
           FunSig {tparams = Mono; params = [];
             rest_param = None; this_param = None;
             return = (Annot (Number [6:24-30]));
             predicate = None; hook = NonHook})
           ]}
    4. TypeAlias {id_loc = [7:15-18];
         name = "Baz"; tparams = Mono;
         body = (Annot (String [7:21-27]))}
    5. EnumBinding {id_loc = [8:7-8];
         name = "B"; rep = StringRep {truthy = true};
         members = { "C" -> [9:4-5]; "D" -> [10:4-5] };
         has_unknown_members = false}
    6. NamespaceBinding {id_loc = [1:18-20];
         name = "ns";
         values =
         { "B" -> ([8:7-8], (Ref LocalRef {ref_loc = [8:7-8]; index = 5}));
           "bar1" -> ([2:23-27], (Ref LocalRef {ref_loc = [2:23-27]; index = 0}));
           "bar2" -> ([3:16-20], (Ref LocalRef {ref_loc = [3:16-20]; index = 1}));
           "bar3" -> ([4:14-18], (Ref LocalRef {ref_loc = [4:14-18]; index = 2}));
           "f" -> ([5:19-20], (Ref LocalRef {ref_loc = [5:19-20]; index = 3})) };
         types = { "Baz" -> ([7:15-18], (Ref LocalRef {ref_loc = [7:15-18]; index = 4})) }}

    Builtin global value ns |}]

let%expect_test "builtin_pattern" =
  print_builtins [{|
    const o = { p: 0 };
    const {p} = o;
  |}];
  [%expect {|
    Local defs:
    0. Variable {id_loc = [1:6-7]; name = "o";
         def =
         (Value
            ObjLit {loc = [1:10-18];
              frozen = false; proto = None;
              props =
              { "p" ->
                (ObjValueField ([1:12-13], (
                   Value (NumberLit ([1:15-16], 0., "0"))), Polarity.Neutral)) }})}
    1. Variable {id_loc = [2:7-8]; name = "p"; def = (Pattern 1)}

    Pattern defs:
    0. (Ref LocalRef {ref_loc = [2:12-13]; index = 0})

    Patterns:
    0. (PDef 0)
    1. PropP {id_loc = [2:7-8]; name = "p"; def = 0}

    Builtin global value o
    Builtin global value p |}]

let%expect_test "this_param_1" =
  print_sig {|
    export function foo(this : mixed) : void {}
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. FunBinding {id_loc = [1:16-19];
         name = "foo"; async = false;
         generator = false; fn_loc = [1:7-40];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = (Some (Annot (Mixed [1:27-32])));
           return = (Annot (Void [1:36-40]));
           predicate = None; hook = NonHook};
         statics = {}} |}]

let%expect_test "this_param_2" =
  print_sig {|
    export class A {
      foo(this : mixed) : void {}
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"A"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ClassBinding {id_loc = [1:13-14];
         name = "A";
         def =
         ClassSig {tparams = Mono; extends = ClassImplicitExtends;
           implements = []; static_props = {};
           proto_props =
           { "foo" ->
             ObjValueMethod {id_loc = [2:2-5];
               fn_loc = [2:2-29]; async = false;
               generator = false;
               def =
               FunSig {tparams = Mono;
                 params = []; rest_param = None;
                 this_param = (Some (Annot (Mixed [2:13-18])));
                 return = (Annot (Void [2:22-26]));
                 predicate = None; hook = NonHook}} };
           own_props = {}}} |}]

let%expect_test "this_param_3" =
  print_sig {|
    declare export function foo(this : mixed) : void;
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareFun {id_loc = [1:24-27];
         name = "foo"; fn_loc = [1:27-48];
         def =
         FunSig {tparams = Mono; params = [];
           rest_param = None; this_param = (Some (Annot (Mixed [1:35-40])));
           return = (Annot (Void [1:44-48]));
           predicate = None; hook = NonHook};
         tail = []} |}]

let%expect_test "this_param_4" =
  print_sig {|
    declare export class A {
      foo(this : mixed) : void;
    }
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"A"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareClassBinding {id_loc = [1:21-22];
         name = "A";
         def =
         DeclareClassSig {tparams = Mono;
           extends = ClassImplicitExtends;
           mixins = []; implements = [];
           static_props = {}; own_props = {};
           proto_props =
           { "foo" ->
             (InterfaceMethod
                (([2:2-5], [2:2-26],
                  FunSig {tparams = Mono;
                    params = []; rest_param = None;
                    this_param = (Some (Annot (Mixed [2:13-18])));
                    return = (Annot (Void [2:22-26]));
                    predicate = None;
                    hook = NonHook}),
                 [])) };
           static_calls = []; calls = [];
           dict = None; static_dict = None}} |}]

let%expect_test "this_param_5" =
  print_sig {|
    export type Foo = (this : mixed) => void
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"Foo"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-15];
         name = "Foo"; tparams = Mono;
         body =
         (Annot
            (FunAnnot ([1:18-40],
               FunSig {tparams = Mono;
                 params = []; rest_param = None;
                 this_param = (Some (Annot (Mixed [1:26-31])));
                 return = (Annot (Void [1:36-40]));
                 predicate = None; hook = NonHook}
               )))} |}]

let%expect_test "this_param_6" =
  print_sig {|
    export type O = { f : (this : mixed) => void, a : number }
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"O"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "O"; tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:16-58];
              obj_kind = InexactObj;
              props =
              { "a" -> (ObjAnnotField ([1:46-47], (Annot (Number [1:50-56])), Polarity.Neutral));
                "f" ->
                (ObjAnnotField ([1:18-19],
                   (Annot
                      (FunAnnot ([1:22-44],
                         FunSig {tparams = Mono;
                           params = [];
                           rest_param = None;
                           this_param = (Some (Annot (Mixed [1:30-35])));
                           return = (Annot (Void [1:40-44]));
                           predicate = None;
                           hook = NonHook}
                         ))),
                   Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})} |}]

let%expect_test "optional_indexed_access" =
  print_sig {|
    export type T = Obj?.['a']['b'];
  |};
  [%expect {|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "T"; tparams = Mono;
         body =
         (Annot
            OptionalIndexedAccessResultType {
              loc = [1:16-31];
              non_maybe_result =
              (Annot
                 ElementType {loc = [1:16-31];
                   obj =
                   (Annot
                      OptionalIndexedAccessNonMaybeType {
                        loc = [1:16-26];
                        obj =
                        (TyRef
                           (Unqualified
                              BuiltinRef {ref_loc = [1:16-19]; type_ref = true; name = "Obj"}));
                        index = (Annot (SingletonString ([1:22-25], "a")))});
                   elem = (Annot (SingletonString ([1:27-30], "b")))});
              void_loc = [1:16-26]})}
  |}]

let%expect_test "cjs_export_type_star" =
  print_sig {|
    export type * from 'foo';
    export type * from 'bar';
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = [([2:12-13], 1); ([1:12-13], 0)];
        strict = true; platform_availability_set = None}}

    Module refs:
    0. foo
    1. bar |}]

let%expect_test "es_export_star" =
  print_sig {|
    export type * from 'foo';
    export type * from 'bar';
    export * from 'baz';
    export * from 'qux';
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [||];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = [([2:12-13], 1); ([1:12-13], 0)];
        export_keys = [||]; stars = [([4:7-8], 3); ([3:7-8], 2)];
        strict = true; platform_availability_set = None}}

    Module refs:
    0. foo
    1. bar
    2. baz
    3. qux |}]

let%expect_test "duplicate_binding" =
  print_sig {|
    import type {T} from 'foo';
    export type T = any;
  |};
  [%expect{|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "duplicate_binding2" =
  print_sig {|
    const foo = 1;
    export var {foo, bar}: {foo: number, bar: number} = {foo: 2, bar: 3};
  |};
  [%expect{|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"bar"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [2:17-20]; name = "bar"; def = (Pattern 1)}

    Pattern defs:
    0. (Annot
          ObjAnnot {loc = [2:23-49];
            obj_kind = InexactObj;
            props =
            { "bar" -> (ObjAnnotField ([2:37-40], (Annot (Number [2:42-48])), Polarity.Neutral));
              "foo" -> (ObjAnnotField ([2:24-27], (Annot (Number [2:29-35])), Polarity.Neutral)) };
            proto = ObjAnnotImplicitProto})

    Patterns:
    0. (PDef 0)
    1. PropP {id_loc = [2:17-20]; name = "bar"; def = 0} |}]

let%expect_test "duplicate_binding2" =
  print_sig {|
    class C {}
    export default class C {}
  |};
  [%expect {|
    CJSModule {type_exports = [||]; exports = None;
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}} |}]

let%expect_test "fun_shadow_declare_fun" =
  print_sig {|
    declare function f(x: string): number;
    declare function f(x: number): string;
    export function f() {}
  |};
  [%expect {|
    ESModule {type_exports = [||]; exports = [|(ExportBinding 0)|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"f"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DeclareFun {id_loc = [1:17-18];
         name = "f"; fn_loc = [1:18-37];
         def =
         FunSig {tparams = Mono;
           params = [FunParam {name = (Some "x"); t = (Annot (String [1:22-28]))}];
           rest_param = None; this_param = None;
           return = (Annot (Number [1:31-37]));
           predicate = None; hook = NonHook};
         tail =
         [([2:17-18], [2:18-37],
           FunSig {tparams = Mono;
             params = [FunParam {name = (Some "x"); t = (Annot (Number [2:22-28]))}];
             rest_param = None; this_param = None;
             return = (Annot (String [2:31-37]));
             predicate = None; hook = NonHook})
           ]} |}]

let%expect_test "optional_tuple_elements" =
  print_sig {|
    export type T = [a: number, b?: string];
  |};
  [%expect{|
    CJSModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "T"; tparams = Mono;
         body =
         (Annot
            Tuple {loc = [1:16-39];
              elems_rev =
              [TupleElement {loc = [1:28-38];
                 name = (Some "b"); t = (Annot (Optional (Annot (String [1:32-38]))));
                 polarity = Polarity.Neutral;
                 optional = true};
                TupleElement {loc = [1:17-26];
                  name = (Some "a");
                  t = (Annot (Number [1:20-26]));
                  polarity = Polarity.Neutral;
                  optional = false}
                ]})}
  |}]

let%expect_test "tuple_spread" =
  print_sig {|
    type S = [string, boolean];
    export type T = [number, ...S];
  |};
  [%expect{|
    CJSModule {type_exports = [|(ExportTypeBinding 1)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "S";
         tparams = Mono;
         body =
         (Annot
            Tuple {loc = [1:9-26];
              elems_rev =
              [TupleElement {loc = [1:18-25];
                 name = None; t = (Annot (Boolean [1:18-25]));
                 polarity = Polarity.Neutral;
                 optional = false};
                TupleElement {loc = [1:10-16];
                  name = None; t = (Annot (String [1:10-16]));
                  polarity = Polarity.Neutral;
                  optional = false}
                ]})}
    1. TypeAlias {id_loc = [2:12-13];
         name = "T"; tparams = Mono;
         body =
         (Annot
            Tuple {loc = [2:16-30];
              elems_rev =
              [TupleSpread {loc = [2:25-29];
                 name = None; t = (TyRef (Unqualified LocalRef {ref_loc = [2:28-29]; index = 0}))};
                TupleElement {loc = [2:17-23];
                  name = None; t = (Annot (Number [2:17-23]));
                  polarity = Polarity.Neutral;
                  optional = false}
                ]})}
  |}]

let%expect_test "mapped_types" =
  print_sig {|
    type O = {foo: number, bar: string};
    export type T1 = {[key in keyof O]: O[key]};
    export type T2 = {[key in keyof O]?: O[key]};
    export type T3 = {+[key in keyof O]: O[key]};
    export type T4 = {-[key in keyof O]?: O[key]};
    export type T5 = {[key in O]: O[key]};
  |};
  [%expect{|
    CJSModule {
      type_exports =
      [|(ExportTypeBinding 1); (ExportTypeBinding 2); (
        ExportTypeBinding 3); (ExportTypeBinding 4); (
        ExportTypeBinding 5)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T1"; "T2"; "T3"; "T4"; "T5"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "O";
         tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:9-35];
              obj_kind = InexactObj;
              props =
              { "bar" -> (ObjAnnotField ([1:23-26], (Annot (String [1:28-34])), Polarity.Neutral));
                "foo" -> (ObjAnnotField ([1:10-13], (Annot (Number [1:15-21])), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
    1. TypeAlias {id_loc = [2:12-14];
         name = "T1"; tparams = Mono;
         body =
         (Annot
            MappedTypeAnnot {loc = [2:18-42];
              source_type = (TyRef (Unqualified LocalRef {ref_loc = [2:32-33]; index = 0}));
              property_type =
              (Annot
                 ElementType {loc = [2:36-42];
                   obj = (TyRef (Unqualified LocalRef {ref_loc = [2:36-37]; index = 0}));
                   elem = (Annot Bound {ref_loc = [2:38-41]; name = "key"})});
              key_tparam =
              TParam {name_loc = [2:19-22];
                name = "key"; polarity = Polarity.Neutral;
                bound = None; default = None};
              variance = Polarity.Neutral;
              optional = Flow_ast.Type.Object.MappedType.NoOptionalFlag;
              inline_keyof = true})}
    2. TypeAlias {id_loc = [3:12-14];
         name = "T2"; tparams = Mono;
         body =
         (Annot
            MappedTypeAnnot {loc = [3:18-43];
              source_type = (TyRef (Unqualified LocalRef {ref_loc = [3:32-33]; index = 0}));
              property_type =
              (Annot
                 ElementType {loc = [3:37-43];
                   obj = (TyRef (Unqualified LocalRef {ref_loc = [3:37-38]; index = 0}));
                   elem = (Annot Bound {ref_loc = [3:39-42]; name = "key"})});
              key_tparam =
              TParam {name_loc = [3:19-22];
                name = "key"; polarity = Polarity.Neutral;
                bound = None; default = None};
              variance = Polarity.Neutral;
              optional = Flow_ast.Type.Object.MappedType.Optional;
              inline_keyof = true})}
    3. TypeAlias {id_loc = [4:12-14];
         name = "T3"; tparams = Mono;
         body =
         (Annot
            MappedTypeAnnot {loc = [4:18-43];
              source_type = (TyRef (Unqualified LocalRef {ref_loc = [4:33-34]; index = 0}));
              property_type =
              (Annot
                 ElementType {loc = [4:37-43];
                   obj = (TyRef (Unqualified LocalRef {ref_loc = [4:37-38]; index = 0}));
                   elem = (Annot Bound {ref_loc = [4:39-42]; name = "key"})});
              key_tparam =
              TParam {name_loc = [4:20-23];
                name = "key"; polarity = Polarity.Neutral;
                bound = None; default = None};
              variance = Polarity.Positive;
              optional = Flow_ast.Type.Object.MappedType.NoOptionalFlag;
              inline_keyof = true})}
    4. TypeAlias {id_loc = [5:12-14];
         name = "T4"; tparams = Mono;
         body =
         (Annot
            MappedTypeAnnot {loc = [5:18-44];
              source_type = (TyRef (Unqualified LocalRef {ref_loc = [5:33-34]; index = 0}));
              property_type =
              (Annot
                 ElementType {loc = [5:38-44];
                   obj = (TyRef (Unqualified LocalRef {ref_loc = [5:38-39]; index = 0}));
                   elem = (Annot Bound {ref_loc = [5:40-43]; name = "key"})});
              key_tparam =
              TParam {name_loc = [5:20-23];
                name = "key"; polarity = Polarity.Neutral;
                bound = None; default = None};
              variance = Polarity.Negative;
              optional = Flow_ast.Type.Object.MappedType.Optional;
              inline_keyof = true})}
    5. TypeAlias {id_loc = [6:12-14];
         name = "T5"; tparams = Mono;
         body =
         (Annot
            MappedTypeAnnot {loc = [6:18-36];
              source_type = (TyRef (Unqualified LocalRef {ref_loc = [6:26-27]; index = 0}));
              property_type =
              (Annot
                 ElementType {loc = [6:30-36];
                   obj = (TyRef (Unqualified LocalRef {ref_loc = [6:30-31]; index = 0}));
                   elem = (Annot Bound {ref_loc = [6:32-35]; name = "key"})});
              key_tparam =
              TParam {name_loc = [6:19-22];
                name = "key"; polarity = Polarity.Neutral;
                bound = None; default = None};
              variance = Polarity.Neutral;
              optional = Flow_ast.Type.Object.MappedType.NoOptionalFlag;
              inline_keyof = false})} |}]

let%expect_test "mapped_types_invalid" =
  print_sig {|
    type O = {foo: number, bar: string};
    export type T = {[key in keyof O]-?: O[key]};
    export type U = {[key in keyof O]: O[key], foo: number};
  |};
  [%expect{|
    CJSModule {type_exports = [|(ExportTypeBinding 0); (ExportTypeBinding 1)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"T"; "U"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [2:12-13]; name = "T"; tparams = Mono; body = (Annot (Any [2:17-43]))}
    1. TypeAlias {id_loc = [3:12-13]; name = "U"; tparams = Mono; body = (Annot (Any [3:16-55]))}
  |}]

let%expect_test "dirtify_defs" =
  print_sig ~locs_to_dirtify:[
    Loc.mk_loc (1,25) (1,26);
    Loc.mk_loc (2,25) (2,26);
    Loc.mk_loc (3,25) (3,26);
  ] {|
export type OOOOOO = {foo: number, bar: string};
export const aaaaa = {foo: 3, bar: ""};
export const {foo} = {foo: 3};
  |}; [%expect{|
    ESModule {type_exports = [|(ExportTypeBinding 0)|];
      exports = [|(ExportBinding 1); (ExportBinding 2)|];
      info =
      ESModuleInfo {type_export_keys = [|"OOOOOO"|];
        type_stars = []; export_keys = [|"aaaaa"; "foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    (dirty) 0. TypeAlias {id_loc = [1:12-18];
                 name = "OOOOOO"; tparams = Mono;
                 body =
                 (Annot
                    ObjAnnot {loc = [1:21-47];
                      obj_kind = InexactObj;
                      props =
                      { "bar" ->
                        (ObjAnnotField ([1:35-38], (Annot (String [1:40-46])), Polarity.Neutral));
                        "foo" ->
                        (ObjAnnotField ([1:22-25], (Annot (Number [1:27-33])), Polarity.Neutral)) };
                      proto = ObjAnnotImplicitProto})}
    (dirty) 1. Variable {id_loc = [2:13-18];
                 name = "aaaaa";
                 def =
                 (Value
                    ObjLit {loc = [2:21-38];
                      frozen = false;
                      proto = None;
                      props =
                      { "bar" ->
                        (ObjValueField ([2:30-33], (
                           Value (StringLit ([2:35-37], ""))), Polarity.Neutral));
                        "foo" ->
                        (ObjValueField ([2:22-25], (
                           Value (NumberLit ([2:27-28], 3., "3"))), Polarity.Neutral)) }})}
    2. Variable {id_loc = [3:14-17]; name = "foo"; def = (Pattern 1)}

    Pattern defs:
    (dirty) 0. (Value
                  ObjLit {loc = [3:21-29];
                    frozen = false; proto = None;
                    props =
                    { "foo" ->
                      (ObjValueField ([3:22-25], (
                         Value (NumberLit ([3:27-28], 3., "3"))), Polarity.Neutral)) }})

    Patterns:
    0. (PDef 0)
    1. PropP {id_loc = [3:14-17]; name = "foo"; def = 0}

  |}]

let%expect_test "component" =
  print_sig {|
    component Baz() {};
    module.exports = { Baz };
  |};
  [%expect{|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [2:17-24];
                 frozen = false; proto = None;
                 props =
                 { "Baz" ->
                   (ObjValueField ([2:19-22], (
                      Ref LocalRef {ref_loc = [2:19-22]; index = 0}), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ComponentBinding {id_loc = [1:10-13];
         name = "Baz"; fn_loc = [1:0-15];
         def =
         ComponentSig {params_loc = [1:13-15];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([1:15],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [1:15]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}

  |}]

let%expect_test "component2" =
  print_sig {|
    type Rest = { x: number }
    component Baz(x: string, ...props: Rest) {};
    module.exports = { Baz };
  |};
  [%expect{|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [3:17-24];
                 frozen = false; proto = None;
                 props =
                 { "Baz" ->
                   (ObjValueField ([3:19-22], (
                      Ref LocalRef {ref_loc = [3:19-22]; index = 1}), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-9]; name = "Rest";
         tparams = Mono;
         body =
         (Annot
            ObjAnnot {loc = [1:12-25];
              obj_kind = InexactObj;
              props =
              { "x" -> (ObjAnnotField ([1:14-15], (Annot (Number [1:17-23])), Polarity.Neutral)) };
              proto = ObjAnnotImplicitProto})}
    1. ComponentBinding {id_loc = [2:10-13];
         name = "Baz"; fn_loc = [2:0-40];
         def =
         ComponentSig {params_loc = [2:13-40];
           tparams = Mono;
           params =
           [ComponentParam {name = "x"; name_loc = [2:14-15]; t = (Annot (String [2:17-23]))}];
           rest_param =
           (Some ComponentRestParam {
                   t = (TyRef (Unqualified LocalRef {ref_loc = [2:35-39]; index = 0}))});
           renders =
           (Annot
              (Renders ([2:40],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [2:40]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}

  |}]

let%expect_test "component3" =
  print_sig {|
    component RadComp() { };
    component Baz('lets go' as x: string, ...props: Rest) renders RadComp {};
    Baz.static = "amazing";
    module.exports = { Baz };
  |};
  [%expect{|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [4:17-24];
                 frozen = false; proto = None;
                 props =
                 { "Baz" ->
                   (ObjValueField ([4:19-22], (
                      Ref LocalRef {ref_loc = [4:19-22]; index = 1}), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ComponentBinding {id_loc = [1:10-17];
         name = "RadComp"; fn_loc = [1:0-19];
         def =
         ComponentSig {params_loc = [1:17-19];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([1:19],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [1:19]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}
    1. ComponentBinding {id_loc = [2:10-13];
         name = "Baz"; fn_loc = [2:0-69];
         def =
         ComponentSig {params_loc = [2:13-53];
           tparams = Mono;
           params =
           [ComponentParam {name = "lets go"; name_loc = [2:14-23]; t = (Annot (String [2:30-36]))}
             ];
           rest_param =
           (Some ComponentRestParam {
                   t =
                   (TyRef
                      (Unqualified BuiltinRef {ref_loc = [2:48-52]; type_ref = true; name = "Rest"}))});
           renders =
           (Annot
              (Renders ([2:54-69], (TyRef (Unqualified LocalRef {ref_loc = [2:62-69]; index = 0})),
                 Flow_ast.Type.Renders.Normal)))}}

  |}]

let%expect_test "component4" =
  print_sig {|
    component Baz<T>(prop: T) renders T {};
    module.exports = { Baz };
  |};
  [%expect{|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [2:17-24];
                 frozen = false; proto = None;
                 props =
                 { "Baz" ->
                   (ObjValueField ([2:19-22], (
                      Ref LocalRef {ref_loc = [2:19-22]; index = 0}), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. ComponentBinding {id_loc = [1:10-13];
         name = "Baz"; fn_loc = [1:0-35];
         def =
         ComponentSig {params_loc = [1:16-25];
           tparams =
           (Poly ([1:13-16],
              TParam {name_loc = [1:14-15];
                name = "T"; polarity = Polarity.Neutral;
                bound = None; default = None},
              []));
           params =
           [ComponentParam {name = "prop";
              name_loc = [1:17-21]; t = (Annot Bound {ref_loc = [1:23-24]; name = "T"})}
             ];
           rest_param = None;
           renders =
           (Annot
              (Renders ([1:26-35], (Annot Bound {ref_loc = [1:34-35]; name = "T"}),
                 Flow_ast.Type.Renders.Normal)))}}
  |}]

let%expect_test "component_5" =
  print_sig {|
    export component Baz () { }
    export default component Bar () { }
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); ExportDefaultBinding {default_loc = [2:7-14]; index = 1}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ComponentBinding {id_loc = [1:17-20];
         name = "Baz"; fn_loc = [1:7-23];
         def =
         ComponentSig {params_loc = [1:21-23];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([1:23],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [1:23]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}
    1. ComponentBinding {id_loc = [2:25-28];
         name = "Bar"; fn_loc = [2:15-31];
         def =
         ComponentSig {params_loc = [2:29-31];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([2:31],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [2:31]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}

  |}]

let%expect_test "component_disabled" =
  print_sig ~enable_component_syntax:false {|
    component Baz() {};
    module.exports = { Baz };
  |};
  [%expect{|
    CJSModule {type_exports = [||];
      exports =
      (Some (Value
               ObjLit {loc = [2:17-24];
                 frozen = false; proto = None;
                 props =
                 { "Baz" ->
                   (ObjValueField ([2:19-22], (
                      Ref LocalRef {ref_loc = [2:19-22]; index = 0}), Polarity.Neutral)) }}));
      info =
      CJSModuleInfo {type_export_keys = [||];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. DisabledComponentBinding {id_loc = [1:10-13]; name = "Baz"}

  |}]

let%expect_test "component_disabled2" =
  print_sig ~enable_component_syntax:false {|
    export component Baz () { }
    export default component Bar () { }
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); ExportDefaultBinding {default_loc = [2:7-14]; index = 1}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DisabledComponentBinding {id_loc = [1:17-20]; name = "Baz"}
    1. DisabledComponentBinding {id_loc = [2:25-28]; name = "Bar"}

  |}]

let%expect_test "declare_component" =
  print_sig {|
    declare export component Baz ();
    declare export default component Bar ();
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); ExportDefaultBinding {default_loc = [2:15-22]; index = 1}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. ComponentBinding {id_loc = [1:25-28];
         name = "Baz"; fn_loc = [1:15-32];
         def =
         ComponentSig {params_loc = [1:29-31];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([1:31],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [1:31]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}}
    1. ComponentBinding {id_loc = [2:33-36];
         name = "Bar"; fn_loc = [2:23-40];
         def =
         ComponentSig {params_loc = [2:37-39];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot
              (Renders ([2:39],
                 (TyRef
                    (Unqualified
                       BuiltinRef {ref_loc = [2:39]; type_ref = true; name = "React$Node"})),
                 Flow_ast.Type.Renders.Normal)))}} |}]

let%expect_test "declare_component_disabled" =
  print_sig ~enable_component_syntax:false {|
    declare export component Baz ();
    declare export default component Bar ();
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports = [|(ExportBinding 0); ExportDefaultBinding {default_loc = [2:15-22]; index = 1}|];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. DisabledComponentBinding {id_loc = [1:25-28]; name = "Baz"}
    1. DisabledComponentBinding {id_loc = [2:33-36]; name = "Bar"} |}]

let%expect_test "component_type" =
  print_sig {|
    type A = number
    type B = string
    declare export var Baz: component(x: A) renders B;
    declare var Bar: component();
    declare export default Bar;
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports =
      [|(ExportBinding 2);
        ExportDefault {default_loc = [5:15-22];
          def = (TyRef (Unqualified LocalRef {ref_loc = [5:23-26]; index = 3}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:5-6]; name = "A"; tparams = Mono; body = (Annot (Number [1:9-15]))}
    1. TypeAlias {id_loc = [2:5-6]; name = "B"; tparams = Mono; body = (Annot (String [2:9-15]))}
    2. Variable {id_loc = [3:19-22];
         name = "Baz";
         def =
         (Annot
            (ComponentAnnot ([3:24-49],
               ComponentSig {params_loc = [3:33-39];
                 tparams = Mono;
                 params =
                 [ComponentParam {name = "x";
                    name_loc = [3:34-35];
                    t = (TyRef (Unqualified LocalRef {ref_loc = [3:37-38]; index = 0}))}
                   ];
                 rest_param = None;
                 renders =
                 (Annot
                    (Renders ([3:40-49],
                       (TyRef (Unqualified LocalRef {ref_loc = [3:48-49]; index = 1})),
                       Flow_ast.Type.Renders.Normal)))}
               )))}
    3. Variable {id_loc = [4:12-15];
         name = "Bar";
         def =
         (Annot
            (ComponentAnnot ([4:17-28],
               ComponentSig {params_loc = [4:26-28];
                 tparams = Mono; params = [];
                 rest_param = None;
                 renders =
                 (Annot
                    (Renders ([4:28],
                       (TyRef
                          (Unqualified
                             BuiltinRef {ref_loc = [4:28]; type_ref = true; name = "React$Node"})),
                       Flow_ast.Type.Renders.Normal)))}
               )))} |}]

let%expect_test "declare_component_disabled" =
  print_sig ~enable_component_syntax:false {|
    declare export var Baz: component();
    declare var Bar: component();
    declare export default Bar;
  |};
  [%expect{|
    ESModule {type_exports = [||];
      exports =
      [|(ExportBinding 0);
        ExportDefault {default_loc = [3:15-22];
          def = (TyRef (Unqualified LocalRef {ref_loc = [3:23-26]; index = 1}))}
        |];
      info =
      ESModuleInfo {type_export_keys = [||];
        type_stars = []; export_keys = [|"Baz"; "default"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. Variable {id_loc = [1:19-22]; name = "Baz"; def = (Annot (Any [1:24-35]))}
    1. Variable {id_loc = [2:12-15]; name = "Bar"; def = (Annot (Any [2:17-28]))} |}]

let%expect_test "render_types" =
  print_sig {|
    export type X = renders number;
    export type Y = renders number | string;
    export type Z = renders (number | string);
  |};
  [%expect{|
    CJSModule {
      type_exports = [|(ExportTypeBinding 0); (ExportTypeBinding 1); (ExportTypeBinding 2)|];
      exports = None;
      info =
      CJSModuleInfo {type_export_keys = [|"X"; "Y"; "Z"|];
        type_stars = []; strict = true;
        platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "X"; tparams = Mono;
         body =
         (Annot (Renders ([1:16-30], (Annot (Number [1:24-30])), Flow_ast.Type.Renders.Normal)))}
    1. TypeAlias {id_loc = [2:12-13];
         name = "Y"; tparams = Mono;
         body =
         (Annot
            Union {loc = [2:16-39];
              t0 =
              (Annot
                 (Renders ([2:16-30], (Annot (Number [2:24-30])), Flow_ast.Type.Renders.Normal)));
              t1 = (Annot (String [2:33-39]));
              ts = []})}
    2. TypeAlias {id_loc = [3:12-13];
         name = "Z"; tparams = Mono;
         body =
         (Annot
            (Renders ([3:16-41],
               (Annot
                  Union {loc = [3:25-40];
                    t0 = (Annot (Number [3:25-31]));
                    t1 = (Annot (String [3:34-40]));
                    ts = []}),
               Flow_ast.Type.Renders.Normal)))} |}]

let%expect_test "render_maybe_types" =
  print_sig {|
    export type X = renders? number;
    export type Y = renders? number | string;
    export type Z = renders? (number | string);
    export type C = component() renders? number;
    declare export component Foo() renders? number;
    export component Bar() renders? number { return null }
  |};
  [%expect{|
    ESModule {
      type_exports =
      [|(ExportTypeBinding 3); (ExportTypeBinding 0); (
        ExportTypeBinding 1); (ExportTypeBinding 2)|];
      exports = [|(ExportBinding 5); (ExportBinding 4)|];
      info =
      ESModuleInfo {type_export_keys = [|"C"; "X"; "Y"; "Z"|];
        type_stars = []; export_keys = [|"Bar"; "Foo"|];
        stars = []; strict = true; platform_availability_set = None}}

    Local defs:
    0. TypeAlias {id_loc = [1:12-13];
         name = "X"; tparams = Mono;
         body =
         (Annot (Renders ([1:16-31], (Annot (Number [1:25-31])), Flow_ast.Type.Renders.Maybe)))}
    1. TypeAlias {id_loc = [2:12-13];
         name = "Y"; tparams = Mono;
         body =
         (Annot
            Union {loc = [2:16-40];
              t0 =
              (Annot (Renders ([2:16-31], (Annot (Number [2:25-31])), Flow_ast.Type.Renders.Maybe)));
              t1 = (Annot (String [2:34-40]));
              ts = []})}
    2. TypeAlias {id_loc = [3:12-13];
         name = "Z"; tparams = Mono;
         body =
         (Annot
            (Renders ([3:16-42],
               (Annot
                  Union {loc = [3:26-41];
                    t0 = (Annot (Number [3:26-32]));
                    t1 = (Annot (String [3:35-41]));
                    ts = []}),
               Flow_ast.Type.Renders.Maybe)))}
    3. TypeAlias {id_loc = [4:12-13];
         name = "C"; tparams = Mono;
         body =
         (Annot
            (ComponentAnnot ([4:16-43],
               ComponentSig {params_loc = [4:25-27];
                 tparams = Mono; params = [];
                 rest_param = None;
                 renders =
                 (Annot
                    (Renders ([4:28-43], (Annot (Number [4:37-43])), Flow_ast.Type.Renders.Maybe)))}
               )))}
    4. ComponentBinding {id_loc = [5:25-28];
         name = "Foo"; fn_loc = [5:15-47];
         def =
         ComponentSig {params_loc = [5:28-30];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot (Renders ([5:31-46], (Annot (Number [5:40-46])), Flow_ast.Type.Renders.Maybe)))}}
    5. ComponentBinding {id_loc = [6:17-20];
         name = "Bar"; fn_loc = [6:7-38];
         def =
         ComponentSig {params_loc = [6:20-22];
           tparams = Mono; params = [];
           rest_param = None;
           renders =
           (Annot (Renders ([6:23-38], (Annot (Number [6:32-38])), Flow_ast.Type.Renders.Maybe)))}} |}]
