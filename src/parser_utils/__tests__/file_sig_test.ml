(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open File_sig

let visit ?parse_options ?(module_ref_prefix = None) source =
  let ast, _ = Parser_flow.program ~parse_options source in
  match program ~ast ~module_ref_prefix with
  | Ok fsig -> fsig
  | Error _ -> assert_failure "Unexpected error"

let visit_err ?parse_options ?(module_ref_prefix = None) source =
  let ast, _ = Parser_flow.program ~parse_options source in
  match program ~ast ~module_ref_prefix with
  | Error e -> e
  | Ok _ -> assert_failure "Unexpected success"

let substring_loc s loc =
  let open Loc in
  let {start={offset=a; _}; _end={offset=b; _}; _} = loc in
  String.sub s a (b - a)

let call_opt x = function Some f -> f x | None -> ()

let assert_es ?assert_named ?assert_star = function
  | ES { named; star } ->
    call_opt named assert_named;
    call_opt star assert_star;
  | CommonJS _ ->
    assert_failure "Unexpected module kind"

let assert_cjs ?assert_export_loc = function
  | CommonJS { mod_exp_loc } ->
    let offsets =
      Option.map mod_exp_loc ~f:Loc.(fun { start={offset=a;_}; _end={offset=b;_}; _} -> (a, b))
    in
    call_opt offsets assert_export_loc
  | ES _ ->
    assert_failure "Unexpected module kind"

let assert_some opt ~f =
  match opt with
  | Some x -> f x
  | None -> assert_failure "Unexpected none"

let assert_singleton_smap ~ctxt key map =
  assert_equal ~ctxt 1 (SMap.cardinal map);
  SMap.find_unsafe key map

let assert_singleton_nel nel =
  match nel with
  | x, [] -> x
  | _ -> assert_failure "Expected singleton nel"

let assert_substring_equal ~ctxt expected source loc =
  assert_equal ~ctxt expected (substring_loc source loc)

let assert_substrings_equal ~ctxt expected_remote expected_local source {remote_loc; local_loc} =
  assert_substring_equal ~ctxt expected_remote source remote_loc;
  assert_substring_equal ~ctxt expected_local source local_loc

let tests = "require" >::: [
  "cjs_require" >:: begin fun ctxt ->
    let source = "const Foo = require('foo')" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = Some (BindIdent (ident_loc, "Foo"))
      }] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "require('foo')" source require_loc;
      assert_substring_equal ~ctxt "Foo" source ident_loc
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_deep_requires" >:: begin fun ctxt ->
    let source = "let foo = {x: require('bar')}; func(foo, require('baz'));" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [ Require {
        source = (baz_loc, "baz");
        require_loc = req_baz_loc;
        bindings = None;
        }
      ; Require {
        source = (bar_loc, "bar");
        require_loc = req_bar_loc;
        bindings = None;
        }
      ] ->
      assert_substring_equal ~ctxt "'bar'" source bar_loc;
      assert_substring_equal ~ctxt "require('bar')" source req_bar_loc;
      assert_substring_equal ~ctxt "'baz'" source baz_loc;
      assert_substring_equal ~ctxt "require('baz')" source req_baz_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_deep_requires_plus_bindings" >:: begin fun ctxt ->
    let source = "const Foo = require('foo'); func(Foo, require('bar'));" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [ Require {
        source = (bar_loc, "bar");
        require_loc = req_bar_loc;
        bindings = None;
        }
      ; Require {
        source = (foo_loc, "foo");
        require_loc = req_foo_loc;
        bindings = Some (BindIdent (foo_id_loc, "Foo"));
        }
      ] ->
      assert_substring_equal ~ctxt "'foo'" source foo_loc;
      assert_substring_equal ~ctxt "require('foo')" source req_foo_loc;
      assert_substring_equal ~ctxt "Foo" source foo_id_loc;
      assert_substring_equal ~ctxt "'bar'" source bar_loc;
      assert_substring_equal ~ctxt "require('bar')" source req_bar_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_require_template_literal" >:: begin fun ctxt ->
    let source = "const Foo = require(`foo`)" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = Some (BindIdent (ident_loc, "Foo"))
      }] ->
      assert_substring_equal ~ctxt "`foo`" source source_loc;
      assert_substring_equal ~ctxt "require(`foo`)" source require_loc;
      assert_substring_equal ~ctxt "Foo" source ident_loc
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_require_named" >:: begin fun ctxt ->
    let source = "const {foo, bar: baz} = require('foo');" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = Some (BindNamed map)
      }] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "require('foo')" source require_loc;
      assert_equal ~ctxt 2 (SMap.cardinal map);
      let foo_loc, foo_loc' = match SMap.find_unsafe "foo" map with
      | locals when SMap.mem "foo" locals ->
        let { local_loc = loc; remote_loc = loc' } = Nel.hd @@ SMap.find "foo" locals in
        loc, loc'
      | _ -> assert_failure "Unexpected requires"
      in
      let baz_loc, bar_loc = match SMap.find_unsafe "bar" map with
      | locals when SMap.mem "baz" locals ->
        let { local_loc = loc; remote_loc = loc' } = Nel.hd @@ SMap.find "baz" locals in
        loc, loc'
      | _ -> assert_failure "Unexpected requires"
      in
      assert_substring_equal ~ctxt "foo" source foo_loc;
      assert_substring_equal ~ctxt "foo" source foo_loc';
      assert_substring_equal ~ctxt "bar" source bar_loc;
      assert_substring_equal ~ctxt "baz" source baz_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_require_duplicate_remote" >:: begin fun ctxt ->
    let source = "const {foo: bar, foo: baz} = require('foo');" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = Some (BindNamed map)
      }] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "require('foo')" source require_loc;
      assert_equal ~ctxt 1 (SMap.cardinal map);
      let bar_loc, foo_loc = match SMap.find_unsafe "foo" map with
      | locals when SMap.mem "bar" locals ->
        let { local_loc = loc; remote_loc = loc' } = Nel.hd @@ SMap.find "bar" locals in
        loc, loc'
      | _ -> assert_failure "Unexpected requires"
      in
      let baz_loc, foo_loc' = match SMap.find_unsafe "foo" map with
      | locals when SMap.mem "baz" locals ->
        let { local_loc = loc; remote_loc = loc' } = Nel.hd @@ SMap.find "baz" locals in
        loc, loc'
      | _ -> assert_failure "Unexpected requires"
      in
      assert_substring_equal ~ctxt "foo" source foo_loc;
      assert_substring_equal ~ctxt "foo" source foo_loc';
      assert_substring_equal ~ctxt "bar" source bar_loc;
      assert_substring_equal ~ctxt "baz" source baz_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_require_duplicate_local" >:: begin fun ctxt ->
    let source = "const {foo: bar, baz: bar} = require('foo');" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = Some (BindNamed map)
      }] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "require('foo')" source require_loc;
      assert_equal ~ctxt 2 (SMap.cardinal map);
      let bar_loc, baz_loc = match SMap.find_unsafe "baz" map with
      | locals when SMap.mem "bar" locals ->
        let { local_loc = loc; remote_loc = loc' } = Nel.hd @@ SMap.find "bar" locals in
        loc, loc'
      | _ -> assert_failure "Unexpected requires"
      in
      assert_substring_equal ~ctxt "bar" source bar_loc;
      assert_substring_equal ~ctxt "baz" source baz_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_require_in_export" >:: begin fun ctxt ->
    (* An initial version of the change to ban non-toplevel exports failed to descend into the RHS
     * of export statements *)
    let source = "module.exports.foo = require('foo');" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        bindings = None;
      }] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "require('foo')" source require_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_module_ref" >:: begin fun ctxt ->
    let source = "moduleRefConsumer('m#foo')" in
    let {module_sig = {requires; _}; _} = visit source ~module_ref_prefix:(Some "m#") in
    match requires with
    | [Require {
        source = (source_loc, "foo");
        require_loc;
        _
      }] ->
      assert_substring_equal ~ctxt "'m#foo'" source source_loc;
      assert_substring_equal ~ctxt "'m#foo'" source require_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "dynamic_import" >:: begin fun ctxt ->
    let source = "import('foo')" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [ImportDynamic {source = (source_loc, "foo"); import_loc}] ->
      assert_substring_equal ~ctxt "'foo'" source source_loc;
      assert_substring_equal ~ctxt "import('foo')" source import_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "dynamic_import_template_literal" >:: begin fun ctxt ->
    let source = "import(`foo`)" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [ImportDynamic {source = (source_loc, "foo"); import_loc}] ->
      assert_substring_equal ~ctxt "`foo`" source source_loc;
      assert_substring_equal ~ctxt "import(`foo`)" source import_loc;
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import" >:: begin fun ctxt ->
    let source = "import 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import0 {source = (loc, "foo")}] ->
      assert_substring_equal ~ctxt "'foo'" source loc
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_default" >:: begin fun ctxt ->
    let source = "import Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); named; _}] ->
      named
      |> assert_singleton_smap ~ctxt "default"
      |> assert_singleton_smap ~ctxt "Foo"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "Foo" "Foo" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_named" >:: begin fun ctxt ->
    let source = "import {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); named; _}] ->
      named
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_renamed" >:: begin fun ctxt ->
    let source = "import {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); named; _}] ->
      named
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "B"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "B" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_named_type" >:: begin fun ctxt ->
    let source = "import {type A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); types; _}] ->
      types
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_named_typeof" >:: begin fun ctxt ->
    let source = "import {typeof A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); typesof; _}] ->
      typesof
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_ns" >:: begin fun ctxt ->
    let source = "import * as Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); ns = Some (loc, "Foo"); _}] ->
      assert_substring_equal ~ctxt "* as Foo" source loc
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_type" >:: begin fun ctxt ->
    let source = "import type A from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); types; _}] ->
      types
      |> assert_singleton_smap ~ctxt "default"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_type_named" >:: begin fun ctxt ->
    let source = "import type {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); types; _}] ->
      types
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_type_renamed" >:: begin fun ctxt ->
    let source = "import type {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); types; _}] ->
      types
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "B"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "B" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_typeof" >:: begin fun ctxt ->
    let source = "import typeof A from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); typesof; _}] ->
      typesof
      |> assert_singleton_smap ~ctxt "default"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_typeof_named" >:: begin fun ctxt ->
    let source = "import typeof {A} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); typesof; _}] ->
      typesof
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "A" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_typeof_renamed" >:: begin fun ctxt ->
    let source = "import typeof {A as B} from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); typesof; _}] ->
      typesof
      |> assert_singleton_smap ~ctxt "A"
      |> assert_singleton_smap ~ctxt "B"
      |> assert_singleton_nel
      |> assert_substrings_equal ~ctxt "A" "B" source
    | _ -> assert_failure "Unexpected requires"
  end;

  "es_import_typesof_ns" >:: begin fun ctxt ->
    let source = "import typeof * as Foo from 'foo'" in
    let {module_sig = {requires; _}; _} = visit source in
    match requires with
    | [Import {source = (_, "foo"); typesof_ns = Some (loc, "Foo"); _}] ->
      assert_substring_equal ~ctxt "* as Foo" source loc
    | _ -> assert_failure "Unexpected requires"
  end;

  "cjs_default" >:: begin fun ctxt ->
    let {module_sig = {module_kind; _}; _} = visit "" in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt None)
  end;

  "cjs_clobber" >:: begin fun ctxt ->
    let source = "module.exports = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "cjs_clobber_rebound" >:: begin fun ctxt ->
    let source = "var module = {}; module.exports = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (None))
  end;

  "cjs_exports_named_rebound" >:: begin fun ctxt ->
    let source = "var module = {}; module.exports.bar = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (None))
  end;

  "cjs_exports_named_rebound2" >:: begin fun ctxt ->
    let source = "var exports = {}; exports.bar = 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (None))
  end;

  "cjs_exports" >:: begin fun ctxt ->
    let source = "exports = {foo: bar}; exports.baz = qux;" in
    let {module_sig = {module_kind; _}; _} = visit source in
    (* TODO report an export loc here *)
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (22, 29)))
  end;

  "cjs_export_named" >:: begin fun ctxt ->
    let source = "module.exports.foo = 0; module.exports.bar = baz;" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "cjs_export_object" >:: begin fun ctxt ->
    let source = "module.exports = {foo: bar, baz: 0, qux};" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "cjs_export_ident" >:: begin fun ctxt ->
    let source = "module.exports = foo;" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "cjs_export_ident_then_props" >:: begin fun ctxt ->
    let source = "module.exports = foo; module.exports.bar = baz;" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "cjs_export_props_then_ident" >:: begin fun ctxt ->
    let source = "module.exports.foo = bar; module.exports = baz;" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
  end;

  "export_named_type" >:: begin fun ctxt ->
    let source = "export type ty = string" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    let type_export =  assert_singleton_smap ~ctxt "ty" type_exports_named in
    match type_export with
    | _, TypeExportNamed { loc; kind = NamedDeclaration } ->
      assert_substring_equal ~ctxt "type ty = string" source loc
    | _ -> assert_failure "Unexpected type export"
  end;

  "export_named_opaque_type" >:: begin fun ctxt ->
    let source = "export opaque type ty = string" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    let type_export =  assert_singleton_smap ~ctxt "ty" type_exports_named in
    match type_export with
    | _, TypeExportNamed { loc; kind = NamedDeclaration } ->
      assert_substring_equal ~ctxt "opaque type ty = string" source loc
    | _ -> assert_failure "Unexpected type export"
  end;

  "export_named_interface" >:: begin fun ctxt ->
    let source = "export interface I {}" in
    let {module_sig = {type_exports_named; _}; _} = visit source in
    let type_export =  assert_singleton_smap ~ctxt "I" type_exports_named in
    match type_export with
    | _, TypeExportNamed { loc; kind = NamedDeclaration } ->
      assert_substring_equal ~ctxt "interface I {}" source loc
    | _ -> assert_failure "Unexpected type export"
  end;

  "export_default_expr" >:: begin fun ctxt ->
    let source = "export default 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = None } ->
        assert_substring_equal ~ctxt "default" source default_loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_default_anon_decl" >:: begin fun ctxt ->
    let source = "export default function() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = None } ->
        assert_substring_equal ~ctxt "default" source default_loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_default_named_func" >:: begin fun ctxt ->
    let source = "export default function foo() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = Some (loc, "foo") } ->
        assert_substring_equal ~ctxt "default" source default_loc;
        assert_substring_equal ~ctxt "foo" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_default_named_class" >:: begin fun ctxt ->
    let source = "export default function C() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = Some (loc, "C") } ->
        assert_substring_equal ~ctxt "default" source default_loc;
        assert_substring_equal ~ctxt "C" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_named_func" >:: begin fun ctxt ->
    let source = "export function foo() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "foo" named in
      match export with
      | _, ExportNamed { loc; kind = NamedDeclaration } ->
        assert_substring_equal ~ctxt "foo" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_named_class" >:: begin fun ctxt ->
    let source = "export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "C" named in
      match export with
      | _, ExportNamed { loc; kind = NamedDeclaration } ->
        assert_substring_equal ~ctxt "C" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_named_vars" >:: begin fun ctxt ->
    let source = "export var x, y = 0, [a] = [], {p} = {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 4 (SMap.cardinal named);
      let x_loc = match SMap.find_unsafe "x" named with
      | _, ExportNamed { loc; kind = NamedDeclaration } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc = match SMap.find_unsafe "y" named with
      | _, ExportNamed { loc; kind = NamedDeclaration } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let a_loc = match SMap.find_unsafe "a" named with
      | _, ExportNamed { loc; kind = NamedDeclaration } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let p_loc = match SMap.find_unsafe "p" named with
      | _, ExportNamed { loc; kind = NamedDeclaration } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_substring_equal ~ctxt "x" source x_loc;
      assert_substring_equal ~ctxt "y" source y_loc;
      assert_substring_equal ~ctxt "a" source a_loc;
      assert_substring_equal ~ctxt "p" source p_loc;
    )
  end;

  "export_named_specs" >:: begin fun ctxt ->
    let source = "export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      let x_loc = match SMap.find_unsafe "x" named with
      | _, ExportNamed { loc; kind = NamedSpecifier ({local = (x_loc, "x"); source = None }) } when
          x_loc = loc -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc, z_loc = match SMap.find_unsafe "z" named with
      | _, ExportNamed { loc; kind = NamedSpecifier ({local = (y_loc, "y"); source = None }) } -> y_loc, loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_substring_equal ~ctxt "x" source x_loc;
      assert_substring_equal ~ctxt "y" source y_loc;
      assert_substring_equal ~ctxt "z" source z_loc;
    )
  end;

  "export_star" >:: begin fun ctxt ->
    let source = "export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_star:(function
      | [_, ExportStar { star_loc; source = (source_loc, "foo") }] ->
        assert_substring_equal ~ctxt "*" source star_loc;
        assert_substring_equal ~ctxt "'foo'" source source_loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "export_ns" >:: begin fun ctxt ->
    let source = "export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "ns" named in
      match export with
      | _, ExportNs { loc; source = (source_loc, "foo"); _ } ->
        assert_substring_equal ~ctxt "ns" source loc;
        assert_substring_equal ~ctxt "'foo'" source source_loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_module.exports" >:: begin fun ctxt ->
    let source = "declare module.exports: ty" in
    let {module_sig = {module_kind; _}; _} = visit source in
    (* TODO use just the `module.exports` location *)
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 26)))
  end;

  "declare_export_default" >:: begin fun ctxt ->
    let source = "declare export default string" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = None } ->
        assert_substring_equal ~ctxt "default" source default_loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_default_func" >:: begin fun ctxt ->
    let source = "declare export default function foo(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = Some (loc, "foo") } ->
        assert_substring_equal ~ctxt "default" source default_loc;
        assert_substring_equal ~ctxt "foo" source loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_default_class" >:: begin fun ctxt ->
    let source = "declare export default class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      let export = assert_singleton_smap ~ctxt "default" named in
      match export with
      | _, ExportDefault { default_loc; local = Some (loc, "C") } ->
        assert_substring_equal ~ctxt "default" source default_loc;
        assert_substring_equal ~ctxt "C" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_named_func" >:: begin fun ctxt ->
    let source = "declare export function foo(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "foo" named in
      match export with
      | _, ExportNamed { loc; kind = NamedDeclaration } ->
        assert_substring_equal ~ctxt "foo" source loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_named_class" >:: begin fun ctxt ->
    let source = "declare export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "C" named in
      match export with
      | _, ExportNamed { loc; kind = NamedDeclaration } ->
        assert_substring_equal ~ctxt "C" source loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_named_var" >:: begin fun ctxt ->
    let source = "declare export var foo: string" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      let export = assert_singleton_smap ~ctxt "foo" named in
      match export with
      | _, ExportNamed { loc; kind = NamedDeclaration } ->
        assert_substring_equal ~ctxt "foo" source loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_named_specs" >:: begin fun ctxt ->
    let source = "declare export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      let x_loc = match SMap.find_unsafe "x" named with
      | _, ExportNamed { loc; kind = NamedSpecifier ({ local = (x_loc, "x"); source = None }) }
          when x_loc = loc -> loc
      | _ -> assert_failure "Unexpected export"
      in
      let y_loc, z_loc = match SMap.find_unsafe "z" named with
      | _, ExportNamed { loc; kind = NamedSpecifier ({ local = (y_loc, "y"); source = None }) } -> y_loc, loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_substring_equal ~ctxt "x" source x_loc;
      assert_substring_equal ~ctxt "y" source y_loc;
      assert_substring_equal ~ctxt "z" source z_loc;
    )
  end;

  "declare_export_star" >:: begin fun ctxt ->
    let source = "declare export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind ~assert_star:(function
      | [_, ExportStar { star_loc; source = (source_loc, "foo") }] ->
        assert_substring_equal ~ctxt "*" source star_loc;
        assert_substring_equal ~ctxt "'foo'" source source_loc;
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_export_ns" >:: begin fun ctxt ->
    let source = "declare export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      let loc, source_loc = match SMap.find_unsafe "ns" named with
      | _, ExportNs { loc; source = (source_loc, "foo"); _ } -> loc, source_loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_substring_equal ~ctxt "ns" source loc;
      assert_substring_equal ~ctxt "'foo'" source source_loc;
    )
  end;

  "declare_module" >:: begin fun ctxt ->
    let source = "declare module foo {}" in
    let {declare_modules; _} = visit source in
    let modules = declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal modules);
    let loc, { requires; module_kind; type_exports_named; type_exports_star; info = () } =
      SMap.find_unsafe "foo" modules in
    assert_substring_equal ~ctxt source source loc;
    assert_equal ~ctxt 0 (List.length requires);
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt None);
    assert_equal ~ctxt 0 (SMap.cardinal type_exports_named);
    assert_equal ~ctxt 0 (List.length type_exports_star);
  end;

  "declare_module_export_type" >:: begin function ctxt ->
    let source = "declare module foo { declare export type bar = string }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { type_exports_named; _ } =
      SMap.find_unsafe "foo" declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports_named);
    let loc = match SMap.find_unsafe "bar" type_exports_named with
    | _, TypeExportNamed { loc; kind = NamedDeclaration } -> loc
    | _ -> assert_failure "Unexpected type export"
    in
    assert_substring_equal ~ctxt "bar" source loc;
  end;

  "declare_module_export_default_expr" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default ty }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      match SMap.find_unsafe "default" named with
      | _, ExportDefault { default_loc; local = None } ->
        assert_substring_equal ~ctxt "default" source default_loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_module_export_default_decl" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default function bar(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      match SMap.find_unsafe "default" named with
      | _, ExportDefault { default_loc; local = Some (loc, "bar") } ->
        assert_substring_equal ~ctxt "default" source default_loc;
        assert_substring_equal ~ctxt "bar" source loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_module_export_name_func" >:: begin fun ctxt ->
    let source = "declare module foo { declare export function bar(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind ~assert_named:(fun named ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      let loc = match SMap.find_unsafe "bar" named with
      | _, ExportNamed { loc; kind = NamedDeclaration } -> loc
      | _ -> assert_failure "Unexpected export"
      in
      assert_substring_equal ~ctxt "bar" source loc
    )
  end;

  "declare_module_export_star" >:: begin fun ctxt ->
    let source = "declare module foo { declare export * from 'bar' }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind ~assert_star:(function
      | [_, ExportStar { star_loc; source = (source_loc, "bar") }] ->
        assert_substring_equal ~ctxt "*" source star_loc;
        assert_substring_equal ~ctxt "'bar'" source source_loc
      | _ -> assert_failure "Unexpected export"
    )
  end;

  "declare_module_declare_module.export" >:: begin fun ctxt ->
    let source = "declare module foo { declare module.exports: ty }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    (* TODO use o0nly the location of `module.exports` *)
    assert_cjs module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (21, 47)))
  end;

  "err_indeterminate_clobber_after_export" >:: begin fun ctxt ->
    let source = "export default 0; module.exports = 0;" in
    match visit_err source with
    | IndeterminateModuleType loc ->
      assert_substring_equal ~ctxt "module.exports" source loc
  end;

  "err_indeterminate_export_after_clobber" >:: begin fun ctxt ->
    let source = "module.exports = 0; export default 0;" in
    match visit_err source with
    | IndeterminateModuleType loc ->
      assert_substring_equal ~ctxt "export default 0;" source loc
  end;
]
