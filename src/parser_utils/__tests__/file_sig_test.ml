(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open File_sig

let visit ?parse_options source =
  let ast, _ = Parser_flow.program ~parse_options source in
  program ast

let substring_loc s loc =
  let open Loc in
  let {start={offset=a; _}; _end={offset=b; _}; _} = loc in
  String.sub s a (b - a)

let call_opt x = function Some f -> f x | None -> ()

let assert_require
  ?assert_loc ?assert_cjs ?assert_es
  ?assert_named ?assert_ns
  ?assert_types
  ?assert_typesof ?assert_typesof_ns
  { loc; cjs_requires; es_imports; named; ns; types; typesof; typesof_ns } =
  call_opt loc assert_loc;
  call_opt cjs_requires assert_cjs;
  call_opt es_imports assert_es;
  call_opt named assert_named;
  call_opt ns assert_ns;
  call_opt types assert_types;
  call_opt typesof assert_typesof;
  call_opt typesof_ns assert_typesof_ns;
  ()

(* Since most of the examples are only one line, this provides a concise way to
 * create a location for assertions *)
let make_loc start end_ =
  let open Loc in
  {
    source = None;
    start = {
      line = 1;
      column = start;
      offset = start;
    };
    _end = {
      line = 1;
      column = end_;
      offset = end_;
    }
  }

let assert_cjs module_kind f =
  match module_kind with
  | ES _ -> assert_failure "Expected CommonJS, got ES"
  | CommonJS { clobbered } -> f clobbered

let assert_es module_kind f =
  match module_kind with
  | CommonJS _ -> assert_failure "Expected ES, got CommonJS"
  | ES { named; star } -> f named star

let tests = "require" >::: [
  "cjs_require" >:: begin fun ctxt ->
    let source = "const Foo = require('foo')" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_loc:(fun loc ->
        assert_equal ~ctxt "'foo'" (substring_loc source loc))
      ~assert_cjs:(fun requires ->
        assert_equal ~ctxt 1 (List.length requires))
  end;

  "es_import" >:: begin fun ctxt ->
    let source = "import 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_loc:(fun loc ->
        assert_equal ~ctxt "'foo'" (substring_loc source loc))
      ~assert_es:(fun imports ->
        assert_equal ~ctxt 1 (List.length imports))
  end;


  "es_import_default" >:: begin fun ctxt ->
    let source = "import Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_named:(fun named ->
        assert_equal ~ctxt 1 (SMap.cardinal named);
        let locals = SMap.find_unsafe "default" named in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 7 10)) (SMap.find "Foo" locals))
  end;

  "es_import_named" >:: begin fun ctxt ->
    let source = "import {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_named:(fun named ->
        assert_equal ~ctxt 1 (SMap.cardinal named);
        let locals = SMap.find_unsafe "A" named in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 8 9)) (SMap.find "A" locals))
  end;

  "es_import_renamed" >:: begin fun ctxt ->
    let source = "import {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_named:(fun named ->
        let locals = SMap.find_unsafe "A" named in
        assert_equal ~ctxt (Nel.one (make_loc 13 14)) (SMap.find "B" locals))
  end;

  "es_import_named_type" >:: begin fun ctxt ->
    let source = "import {type A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_types:(fun types ->
        assert_equal ~ctxt 1 (SMap.cardinal types);
        let locals = SMap.find_unsafe "A" types in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 13 14)) (SMap.find "A" locals))
  end;

  "es_import_named_typeof" >:: begin fun ctxt ->
    let source = "import {typeof A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_typesof:(fun typesof ->
        assert_equal ~ctxt 1 (SMap.cardinal typesof);
        let locals = SMap.find_unsafe "A" typesof in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 15 16)) (SMap.find "A" locals))
  end;

  "es_import_ns" >:: begin fun ctxt ->
    let source = "import * as Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_ns:(fun ns ->
        assert_equal ~ctxt 1 (SMap.cardinal ns);
        let loc, rest = SMap.find_unsafe "Foo" ns in
        assert_equal ~ctxt 0 (List.length rest);
        assert_equal "* as Foo" (substring_loc source loc))
  end;

  "es_import_type" >:: begin fun ctxt ->
    let source = "import type A from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_types:(fun types ->
        assert_equal ~ctxt 1 (SMap.cardinal types);
        let locals = SMap.find_unsafe "default" types in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 12 13)) (SMap.find "A" locals))
  end;

  "es_import_type_named" >:: begin fun ctxt ->
    let source = "import type {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_types:(fun types ->
        assert_equal ~ctxt 1 (SMap.cardinal types);
        let locals = SMap.find_unsafe "A" types in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 13 14)) (SMap.find "A" locals))
  end;

  "es_import_type_renamed" >:: begin fun ctxt ->
    let source = "import type {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_types:(fun types ->
        let locals = SMap.find_unsafe "A" types in
        assert_equal ~ctxt (Nel.one (make_loc 18 19)) (SMap.find "B" locals))
  end;

  "es_import_typeof" >:: begin fun ctxt ->
    let source = "import typeof A from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_typesof:(fun typesof ->
        assert_equal ~ctxt 1 (SMap.cardinal typesof);
        let locals = SMap.find_unsafe "default" typesof in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 14 15)) (SMap.find "A" locals))
  end;

  "es_import_typeof_named" >:: begin fun ctxt ->
    let source = "import typeof {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_typesof:(fun typesof ->
        assert_equal ~ctxt 1 (SMap.cardinal typesof);
        let locals = SMap.find_unsafe "A" typesof in
        assert_equal ~ctxt 1 (SMap.cardinal locals);
        assert_equal ~ctxt (Nel.one (make_loc 15 16)) (SMap.find "A" locals))
  end;

  "es_import_typeof_renamed" >:: begin fun ctxt ->
    let source = "import typeof {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_typesof:(fun typesof ->
        let locals = SMap.find_unsafe "A" typesof in
        assert_equal ~ctxt (Nel.one (make_loc 20 21)) (SMap.find "B" locals))
  end;

  "es_import_typesof_ns" >:: begin fun ctxt ->
    let source = "import typeof * as Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_typesof_ns:(fun typesof_ns ->
        assert_equal ~ctxt 1 (SMap.cardinal typesof_ns);
        let loc, rest = SMap.find_unsafe "Foo" typesof_ns in
        assert_equal ~ctxt 0 (List.length rest);
        assert_equal "* as Foo" (substring_loc source loc))
  end;

  "cjs_default" >:: begin fun ctxt ->
    let {module_sig = {module_kind; _}; _} = visit "" in
    assert_cjs module_kind (assert_equal ~ctxt None)
  end;

  "cjs_clobber" >:: begin fun ctxt ->
    let source = "module.exports = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind (function
      | None -> assert_failure "Expected clobbered exports"
      | Some loc ->
        assert_equal ~ctxt "module.exports" (substring_loc source loc)
    )
  end;

  "cjs_clobber_rebound" >:: begin fun ctxt ->
    let source = "var module = {}; module.exports = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind (
      assert_equal ~ctxt None
    )
  end;

  "export_named_type" >:: begin fun ctxt ->
    let source = "export type ty = string" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    assert_equal 1 (SMap.cardinal type_exports_named);
    let loc = match SMap.find_unsafe "ty" type_exports_named with
    | TypeExportNamed { loc; local = None; source = None } -> loc
    | _ -> assert_failure "Unexpected type export"
    in
    assert_equal ~ctxt "type ty = string" (substring_loc source loc)
  end;

  "export_named_opaque_type" >:: begin fun ctxt ->
    let source = "export opaque type ty = string" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports_named);
    let loc = match SMap.find_unsafe "ty" type_exports_named with
    | TypeExportNamed { loc; local = None; source = None } -> loc
    | _ -> assert_failure "Unexpected type export"
    in
    assert_equal ~ctxt "opaque type ty = string" (substring_loc source loc)
  end;

  "export_named_interface" >:: begin fun ctxt ->
    let source = "export interface I {}" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports_named);
    let loc = match SMap.find_unsafe "I" type_exports_named with
    | TypeExportNamed { loc; local = None; source = None } -> loc
    | _ -> assert_failure "Unexpected type export"
    in
    assert_equal ~ctxt "interface I {}" (substring_loc source loc)
  end;

  "export_default_expr" >:: begin fun ctxt ->
    let source = "export default 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      match SMap.find_unsafe "default" named with
      | ExportDefault { local = None } -> () (* pass *)
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_default_anon_decl" >:: begin fun ctxt ->
    let source = "export default function() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      match SMap.find_unsafe "default" named with
      | ExportDefault { local = None } -> () (* pass *)
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_default_named_func" >:: begin fun ctxt ->
    let source = "export default function foo() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      let loc = match SMap.find_unsafe "default" named with
      | ExportDefault { local = Some (loc, "foo") } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "foo" (substring_loc source loc)
    )
  end;

  "export_default_named_class" >:: begin fun ctxt ->
    let source = "export default function C() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      let loc = match SMap.find_unsafe "default" named with
      | ExportDefault { local = Some (loc, "C") } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "C" (substring_loc source loc)
    )
  end;

  "export_named_func" >:: begin fun ctxt ->
    let source = "export function foo() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "foo" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "foo" (substring_loc source loc)
    )
  end;

  "export_named_class" >:: begin fun ctxt ->
    let source = "export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "C" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "C" (substring_loc source loc)
    )
  end;

  "export_named_vars" >:: begin fun ctxt ->
    let source = "export var x, y = 0, [a] = [], {p} = {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 4 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let x_loc = match SMap.find_unsafe "x" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc = match SMap.find_unsafe "y" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let a_loc = match SMap.find_unsafe "a" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let p_loc = match SMap.find_unsafe "p" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "x" (substring_loc source x_loc);
      assert_equal ~ctxt "y" (substring_loc source y_loc);
      assert_equal ~ctxt "a" (substring_loc source a_loc);
      assert_equal ~ctxt "p" (substring_loc source p_loc)
    )
  end;

  "export_named_specs" >:: begin fun ctxt ->
    let source = "export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let x_loc = match SMap.find_unsafe "x" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc, z_loc = match SMap.find_unsafe "z" named with
      | ExportNamed { loc; local = Some (y_loc, "y"); source = None } -> y_loc, loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "x" (substring_loc source x_loc);
      assert_equal ~ctxt "y" (substring_loc source y_loc);
      assert_equal ~ctxt "z" (substring_loc source z_loc);
    )
  end;

  "export_star" >:: begin fun ctxt ->
    let source = "export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal star);
      let ExportStar { star_loc; source_loc } = SMap.find_unsafe "foo" star in
      assert_equal ~ctxt "*" (substring_loc source star_loc);
      assert_equal ~ctxt "'foo'" (substring_loc source source_loc);
    )
  end;

  "export_ns" >:: begin fun ctxt ->
    let source = "export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc, source_loc = match SMap.find_unsafe "ns" named with
      | ExportNs { loc; source = (source_loc, "foo") } -> loc, source_loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "ns" (substring_loc source loc);
      assert_equal ~ctxt "'foo'" (substring_loc source source_loc);
    )
  end;

  "declare_module.exports" >:: begin fun ctxt ->
    let source = "declare module.exports: ty" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind (function
      | None -> assert_failure "Expected clobbered exports"
      | Some loc ->
        assert_equal ~ctxt "declare module.exports: ty"
          (substring_loc source loc)
    )
  end;

  "declare_export_default" >:: begin fun ctxt ->
    let source = "declare export default string" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      match SMap.find_unsafe "default" named with
      | ExportDefault { local = None } -> () (* pass *)
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_default_func" >:: begin fun ctxt ->
    let source = "declare export default function foo(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "default" named with
      | ExportDefault { local = Some (loc, "foo") } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "foo" (substring_loc source loc);
    )
  end;

  "declare_export_default_class" >:: begin fun ctxt ->
    let source = "declare export default class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "default" named with
      | ExportDefault { local = Some (loc, "C") } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "C" (substring_loc source loc);
    )
  end;

  "declare_export_named_func" >:: begin fun ctxt ->
    let source = "declare export function foo(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "foo" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "foo" (substring_loc source loc);
    )
  end;

  "declare_export_named_class" >:: begin fun ctxt ->
    let source = "declare export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "C" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "C" (substring_loc source loc);
    )
  end;

  "declare_export_named_var" >:: begin fun ctxt ->
    let source = "declare export var foo: string" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "foo" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "foo" (substring_loc source loc);
    )
  end;

  "declare_export_named_specs" >:: begin fun ctxt ->
    let source = "declare export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let x_loc = match SMap.find_unsafe "x" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc, z_loc = match SMap.find_unsafe "z" named with
      | ExportNamed { loc; local = Some (y_loc, "y"); source = None } -> y_loc, loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "x" (substring_loc source x_loc);
      assert_equal ~ctxt "y" (substring_loc source y_loc);
      assert_equal ~ctxt "z" (substring_loc source z_loc);
    )
  end;

  "declare_export_star" >:: begin fun ctxt ->
    let source = "declare export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal star);
      let ExportStar { star_loc; source_loc } = SMap.find_unsafe "foo" star in
      assert_equal ~ctxt "*" (substring_loc source star_loc);
      assert_equal ~ctxt "'foo'" (substring_loc source source_loc);
    )
  end;

  "declare_export_ns" >:: begin fun ctxt ->
    let source = "declare export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc, source_loc = match SMap.find_unsafe "ns" named with
      | ExportNs { loc; source = (source_loc, "foo") } -> loc, source_loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "ns" (substring_loc source loc);
      assert_equal ~ctxt "'foo'" (substring_loc source source_loc);
    )
  end;

  "declare_module" >:: begin fun ctxt ->
    let source = "declare module foo {}" in
    let {declare_modules; _} = visit source in
    let modules = declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal modules);
    let loc, { requires; module_kind; type_exports_named; type_exports_star } =
      SMap.find_unsafe "foo" modules in
    assert_equal ~ctxt source (substring_loc source loc);
    assert_equal ~ctxt 0 (SMap.cardinal requires);
    assert_cjs module_kind (assert_equal ~ctxt None);
    assert_equal ~ctxt 0 (SMap.cardinal type_exports_named);
    assert_equal ~ctxt 0 (SMap.cardinal type_exports_star);
  end;

  "declare_module_export_type" >:: begin function ctxt ->
    let source = "declare module foo { declare export type bar = string }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { type_exports_named; _ } =
      SMap.find_unsafe "foo" declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports_named);
    let loc = match SMap.find_unsafe "bar" type_exports_named with
    | TypeExportNamed { loc; local = None; source = None } -> loc
    | _ -> assert_failure "Unexpected type export"
    in
    assert_equal ~ctxt "bar" (substring_loc source loc);
  end;

  "declare_module_export_default_expr" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default ty }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      match SMap.find_unsafe "default" named with
      | ExportDefault { local = None } -> () (* pass *)
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_module_export_default_decl" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default function bar(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "default" named with
      | ExportDefault { local = Some (loc, "bar") } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "bar" (substring_loc source loc)
    )
  end;

  "declare_module_export_name_func" >:: begin fun ctxt ->
    let source = "declare module foo { declare export function bar(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal star);
      let loc = match SMap.find_unsafe "bar" named with
      | ExportNamed { loc; local = None; source = None } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_equal ~ctxt "bar" (substring_loc source loc)
    )
  end;

  "declare_module_export_star" >:: begin fun ctxt ->
    let source = "declare module foo { declare export * from 'bar' }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named star ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal star);
      let ExportStar { star_loc; source_loc } = SMap.find_unsafe "bar" star in
      assert_equal ~ctxt "*" (substring_loc source star_loc);
      assert_equal ~ctxt "'bar'" (substring_loc source source_loc)
    )
  end;

  "declare_module_declare_module.export" >:: begin fun ctxt ->
    let source = "declare module foo { declare module.exports: ty }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_cjs module_kind (function
      | None -> assert_failure "Expected clobbered exports"
      | Some loc ->
        assert_equal ~ctxt "declare module.exports: ty"
          (substring_loc source loc)
    )
  end;

  "merge_requires_cjs" >:: begin fun ctxt ->
    let source = "require('foo'); require('foo')" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_cjs:(function
        | [loc1; loc2] when loc1 <> loc2 ->
          assert_equal ~ctxt "'foo'" (substring_loc source loc1);
          assert_equal ~ctxt "'foo'" (substring_loc source loc2);
        | _ -> assert_failure "unexpected cjs requires")
  end;

  "merge_requires_es" >:: begin fun ctxt ->
    let source = "import 'foo'; import 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal requires);
    let require = SMap.find_unsafe "foo" requires in
    assert_require require
      ~assert_es:(function
        | [loc1; loc2] when loc1 <> loc2 ->
          assert_equal ~ctxt "'foo'" (substring_loc source loc1);
          assert_equal ~ctxt "'foo'" (substring_loc source loc2);
        | _ -> assert_failure "unexpected es requires")
  end;
]
