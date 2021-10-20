(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open File_sig.With_Loc

let visit ?parse_options ?(opts = default_opts) source =
  let (ast, _) = Parser_flow.program ~parse_options source in
  match program ~ast ~opts with
  | Ok (fsig, _) -> fsig
  | Error _ -> assert_failure "Unexpected error"

let visit_err ?parse_options ?(opts = default_opts) source =
  let (ast, _) = Parser_flow.program ~parse_options source in
  match program ~ast ~opts with
  | Error e -> e
  | Ok _ -> assert_failure "Unexpected success"

let substring_loc s loc =
  Loc.(
    let table = Offset_utils.make ~kind:Offset_utils.Utf8 s in
    let a = Offset_utils.offset table loc.start in
    let b = Offset_utils.offset table loc._end in
    String.sub s a (b - a)
  )

let call_opt x = function
  | Some f -> f x
  | None -> ()

let assert_es ?assert_named ?assert_star = function
  | ES { named; star } ->
    call_opt named assert_named;
    call_opt star assert_star
  | CommonJS _ -> assert_failure "Unexpected module kind"

let assert_cjs ~source ?assert_export_loc = function
  | CommonJS { mod_exp_loc } ->
    let table = Offset_utils.make ~kind:Offset_utils.Utf8 source in
    let offset_pair_of_loc loc =
      Loc.(Offset_utils.offset table loc.start, Offset_utils.offset table loc._end)
    in
    let offsets = Base.Option.map mod_exp_loc ~f:offset_pair_of_loc in
    call_opt offsets assert_export_loc
  | ES _ -> assert_failure "Unexpected module kind"

let assert_some opt ~f =
  match opt with
  | Some x -> f x
  | None -> assert_failure "Unexpected none"

let assert_singleton_smap ~ctxt key map =
  assert_equal ~ctxt 1 (SMap.cardinal map);
  SMap.find key map

let assert_singleton_nel nel =
  match nel with
  | (x, []) -> x
  | _ -> assert_failure "Expected singleton nel"

let assert_singleton_assoc ~ctxt key assoc =
  match assoc with
  | (x, v) :: _ ->
    assert_equal ~ctxt key x;
    v
  | _ -> assert_failure "Expected singleton list"

let assert_substring_equal ~ctxt expected source loc =
  assert_equal ~ctxt expected (substring_loc source loc)

let assert_substrings_equal ~ctxt expected_remote expected_local source { remote_loc; local_loc } =
  assert_substring_equal ~ctxt expected_remote source remote_loc;
  assert_substring_equal ~ctxt expected_local source local_loc

let tests =
  "require"
  >::: [
         ( "cjs_require" >:: fun ctxt ->
           let source = "const Foo = require('foo')" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require
              {
                source = (source_loc, "foo");
                require_loc;
                bindings = Some (BindIdent (ident_loc, "Foo"));
              };
           ] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "require('foo')" source require_loc;
             assert_substring_equal ~ctxt "Foo" source ident_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_deep_requires" >:: fun ctxt ->
           let source = "let foo = {x: require('bar')}; func(foo, require('baz'));" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require { source = (baz_loc, "baz"); require_loc = req_baz_loc; bindings = None };
            Require { source = (bar_loc, "bar"); require_loc = req_bar_loc; bindings = None };
           ] ->
             assert_substring_equal ~ctxt "'bar'" source bar_loc;
             assert_substring_equal ~ctxt "require('bar')" source req_bar_loc;
             assert_substring_equal ~ctxt "'baz'" source baz_loc;
             assert_substring_equal ~ctxt "require('baz')" source req_baz_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_deep_requires_plus_bindings" >:: fun ctxt ->
           let source = "const Foo = require('foo'); func(Foo, require('bar'));" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require { source = (bar_loc, "bar"); require_loc = req_bar_loc; bindings = None };
            Require
              {
                source = (foo_loc, "foo");
                require_loc = req_foo_loc;
                bindings = Some (BindIdent (foo_id_loc, "Foo"));
              };
           ] ->
             assert_substring_equal ~ctxt "'foo'" source foo_loc;
             assert_substring_equal ~ctxt "require('foo')" source req_foo_loc;
             assert_substring_equal ~ctxt "Foo" source foo_id_loc;
             assert_substring_equal ~ctxt "'bar'" source bar_loc;
             assert_substring_equal ~ctxt "require('bar')" source req_bar_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_require_template_literal" >:: fun ctxt ->
           let source = "const Foo = require(`foo`)" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require
              {
                source = (source_loc, "foo");
                require_loc;
                bindings = Some (BindIdent (ident_loc, "Foo"));
              };
           ] ->
             assert_substring_equal ~ctxt "`foo`" source source_loc;
             assert_substring_equal ~ctxt "require(`foo`)" source require_loc;
             assert_substring_equal ~ctxt "Foo" source ident_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_require_named" >:: fun ctxt ->
           let source = "const {foo, bar: baz} = require('foo');" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require { source = (source_loc, "foo"); require_loc; bindings = Some (BindNamed map) };
           ] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "require('foo')" source require_loc;
             assert_equal ~ctxt 2 (List.length map);
             let (foo_loc, foo_loc') =
               match List.find_all (fun ((_, x), _) -> x = "foo") map with
               | [((foo_loc', _), BindIdent (foo_loc, "foo"))] -> (foo_loc, foo_loc')
               | _ -> assert_failure "Unexpected requires"
             in
             let (baz_loc, bar_loc) =
               match List.find_all (fun ((_, x), _) -> x = "bar") map with
               | [((bar_loc, _), BindIdent (baz_loc, "baz"))] -> (baz_loc, bar_loc)
               | _ -> assert_failure "Unexpected requires"
             in
             assert_substring_equal ~ctxt "foo" source foo_loc;
             assert_substring_equal ~ctxt "foo" source foo_loc';
             assert_substring_equal ~ctxt "bar" source bar_loc;
             assert_substring_equal ~ctxt "baz" source baz_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_require_duplicate_remote" >:: fun ctxt ->
           let source = "const {foo: bar, foo: baz} = require('foo');" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require { source = (source_loc, "foo"); require_loc; bindings = Some (BindNamed map) };
           ] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "require('foo')" source require_loc;
             assert_equal ~ctxt 2 (List.length map);
             let (bar_loc, foo_loc, baz_loc, foo_loc') =
               match List.find_all (fun ((_, x), _) -> x = "foo") map with
               | [
                ((foo_loc', _), BindIdent (baz_loc, "baz"));
                ((foo_loc, _), BindIdent (bar_loc, "bar"));
               ] ->
                 (bar_loc, foo_loc, baz_loc, foo_loc')
               | _ -> assert_failure "Unexpected requires"
             in
             assert_substring_equal ~ctxt "foo" source foo_loc;
             assert_substring_equal ~ctxt "foo" source foo_loc';
             assert_substring_equal ~ctxt "bar" source bar_loc;
             assert_substring_equal ~ctxt "baz" source baz_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_require_duplicate_local" >:: fun ctxt ->
           let source = "const {foo: bar, baz: bar} = require('foo');" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [
            Require { source = (source_loc, "foo"); require_loc; bindings = Some (BindNamed map) };
           ] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "require('foo')" source require_loc;
             assert_equal ~ctxt 2 (List.length map);
             let (bar_loc, baz_loc) =
               match List.find_all (fun ((_, x), _) -> x = "baz") map with
               | [((baz_loc, _), BindIdent (bar_loc, "bar"))] -> (bar_loc, baz_loc)
               | _ -> assert_failure "Unexpected requires"
             in
             assert_substring_equal ~ctxt "bar" source bar_loc;
             assert_substring_equal ~ctxt "baz" source baz_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_require_in_export" >:: fun ctxt ->
           (* An initial version of the change to ban non-toplevel exports failed to descend into the RHS
            * of export statements *)
           let source = "module.exports.foo = require('foo');" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Require { source = (source_loc, "foo"); require_loc; bindings = None }] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "require('foo')" source require_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_module_ref" >:: fun ctxt ->
           let source = "moduleRefConsumer('m#foo')" in
           let { module_sig = { requires; _ }; _ } =
             visit source ~opts:{ default_opts with module_ref_prefix = Some "m#" }
           in
           match requires with
           | [Require { source = (source_loc, "foo"); require_loc; _ }] ->
             assert_substring_equal ~ctxt "'m#foo'" source source_loc;
             assert_substring_equal ~ctxt "'m#foo'" source require_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "relay_integration" >:: fun ctxt ->
           let source = "graphql`query foo {}`" in
           let { module_sig = { requires; _ }; _ } =
             visit source ~opts:{ default_opts with enable_relay_integration = true }
           in
           match requires with
           | [Require { source = (source_loc, "foo.graphql"); require_loc; _ }] ->
             assert_substring_equal ~ctxt "graphql`query foo {}`" source source_loc;
             assert_substring_equal ~ctxt "graphql`query foo {}`" source require_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "relay_integration_module_prefix" >:: fun ctxt ->
           let source = "graphql`query foo {}`" in
           let { module_sig = { requires; _ }; _ } =
             visit
               source
               ~opts:
                 {
                   default_opts with
                   enable_relay_integration = true;
                   relay_integration_module_prefix = Some "./__generated__/";
                 }
           in
           match requires with
           | [Require { source = (source_loc, "./__generated__/foo.graphql"); require_loc; _ }] ->
             assert_substring_equal ~ctxt "graphql`query foo {}`" source source_loc;
             assert_substring_equal ~ctxt "graphql`query foo {}`" source require_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "dynamic_import" >:: fun ctxt ->
           let source = "import('foo')" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [ImportDynamic { source = (source_loc, "foo"); import_loc }] ->
             assert_substring_equal ~ctxt "'foo'" source source_loc;
             assert_substring_equal ~ctxt "import('foo')" source import_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "dynamic_import_template_literal" >:: fun ctxt ->
           let source = "import(`foo`)" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [ImportDynamic { source = (source_loc, "foo"); import_loc }] ->
             assert_substring_equal ~ctxt "`foo`" source source_loc;
             assert_substring_equal ~ctxt "import(`foo`)" source import_loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import" >:: fun ctxt ->
           let source = "import 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import0 { source = (loc, "foo") }] -> assert_substring_equal ~ctxt "'foo'" source loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_default" >:: fun ctxt ->
           let source = "import Foo from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); named; _ }] ->
             named
             |> assert_singleton_smap ~ctxt "default"
             |> assert_singleton_smap ~ctxt "Foo"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "Foo" "Foo" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_named" >:: fun ctxt ->
           let source = "import {A} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); named; _ }] ->
             named
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_renamed" >:: fun ctxt ->
           let source = "import {A as B} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); named; _ }] ->
             named
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "B"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "B" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_named_type" >:: fun ctxt ->
           let source = "import {type A} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); types; _ }] ->
             types
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_named_typeof" >:: fun ctxt ->
           let source = "import {typeof A} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); typesof; _ }] ->
             typesof
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_ns" >:: fun ctxt ->
           let source = "import * as Foo from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); ns = Some (loc, "Foo"); _ }] ->
             assert_substring_equal ~ctxt "Foo" source loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_type" >:: fun ctxt ->
           let source = "import type A from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); types; _ }] ->
             types
             |> assert_singleton_smap ~ctxt "default"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_type_named" >:: fun ctxt ->
           let source = "import type {A} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); types; _ }] ->
             types
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_type_renamed" >:: fun ctxt ->
           let source = "import type {A as B} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); types; _ }] ->
             types
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "B"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "B" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_typeof" >:: fun ctxt ->
           let source = "import typeof A from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); typesof; _ }] ->
             typesof
             |> assert_singleton_smap ~ctxt "default"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_typeof_named" >:: fun ctxt ->
           let source = "import typeof {A} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); typesof; _ }] ->
             typesof
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "A" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_typeof_renamed" >:: fun ctxt ->
           let source = "import typeof {A as B} from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); typesof; _ }] ->
             typesof
             |> assert_singleton_smap ~ctxt "A"
             |> assert_singleton_smap ~ctxt "B"
             |> assert_singleton_nel
             |> assert_substrings_equal ~ctxt "A" "B" source
           | _ -> assert_failure "Unexpected requires"
         );
         ( "es_import_typesof_ns" >:: fun ctxt ->
           let source = "import typeof * as Foo from 'foo'" in
           let { module_sig = { requires; _ }; _ } = visit source in
           match requires with
           | [Import { source = (_, "foo"); typesof_ns = Some (loc, "Foo"); _ }] ->
             assert_substring_equal ~ctxt "Foo" source loc
           | _ -> assert_failure "Unexpected requires"
         );
         ( "cjs_default" >:: fun ctxt ->
           let source = "" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt None)
         );
         ( "cjs_clobber" >:: fun ctxt ->
           let source = "module.exports = 0" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "cjs_clobber_rebound" >:: fun ctxt ->
           let source = "var module = {}; module.exports = 0" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt None)
         );
         ( "cjs_exports_named_rebound" >:: fun ctxt ->
           let source = "var module = {}; module.exports.bar = 0" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt None)
         );
         ( "cjs_exports_named_rebound2" >:: fun ctxt ->
           let source = "var exports = {}; exports.bar = 0" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt None)
         );
         ( "cjs_exports" >:: fun ctxt ->
           let source = "exports = {foo: bar}; exports.baz = qux;" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           (* TODO report an export loc here *)
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (22, 29)))
         );
         ( "cjs_export_named" >:: fun ctxt ->
           let source = "module.exports.foo = 0; module.exports.bar = baz;" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "cjs_export_object" >:: fun ctxt ->
           let source = "module.exports = {foo: bar, baz: 0, qux};" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "cjs_export_ident" >:: fun ctxt ->
           let source = "module.exports = foo;" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "cjs_export_ident_then_props" >:: fun ctxt ->
           let source = "module.exports = foo; module.exports.bar = baz;" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "cjs_export_props_then_ident" >:: fun ctxt ->
           let source = "module.exports.foo = bar; module.exports = baz;" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 14)))
         );
         ( "export_named_type" >:: fun ctxt ->
           let source = "export type ty = string" in
           let { module_sig = { type_exports_named; _ }; _ } = visit source in
           let type_export = assert_singleton_assoc ~ctxt "ty" type_exports_named in
           match type_export with
           | (_, TypeExportNamed { loc; kind = NamedDeclaration }) ->
             assert_substring_equal ~ctxt "type ty = string" source loc
           | _ -> assert_failure "Unexpected type export"
         );
         ( "export_named_opaque_type" >:: fun ctxt ->
           let source = "export opaque type ty = string" in
           let { module_sig = { type_exports_named; _ }; _ } = visit source in
           let type_export = assert_singleton_assoc ~ctxt "ty" type_exports_named in
           match type_export with
           | (_, TypeExportNamed { loc; kind = NamedDeclaration }) ->
             assert_substring_equal ~ctxt "opaque type ty = string" source loc
           | _ -> assert_failure "Unexpected type export"
         );
         ( "export_named_interface" >:: fun ctxt ->
           let source = "export interface I {}" in
           let { module_sig = { type_exports_named; _ }; _ } = visit source in
           let type_export = assert_singleton_assoc ~ctxt "I" type_exports_named in
           match type_export with
           | (_, TypeExportNamed { loc; kind = NamedDeclaration }) ->
             assert_substring_equal ~ctxt "interface I {}" source loc
           | _ -> assert_failure "Unexpected type export"
         );
         ( "export_default_expr" >:: fun ctxt ->
           let source = "export default 0" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = None }) ->
                 assert_substring_equal ~ctxt "default" source default_loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_default_anon_decl" >:: fun ctxt ->
           let source = "export default function() {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = None }) ->
                 assert_substring_equal ~ctxt "default" source default_loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_default_named_func" >:: fun ctxt ->
           let source = "export default function foo() {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = Some (loc, "foo") }) ->
                 assert_substring_equal ~ctxt "default" source default_loc;
                 assert_substring_equal ~ctxt "foo" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_default_named_class" >:: fun ctxt ->
           let source = "export default function C() {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = Some (loc, "C") }) ->
                 assert_substring_equal ~ctxt "default" source default_loc;
                 assert_substring_equal ~ctxt "C" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_named_func" >:: fun ctxt ->
           let source = "export function foo() {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "foo" named in
               match export with
               | (_, ExportNamed { loc; kind = NamedDeclaration }) ->
                 assert_substring_equal ~ctxt "foo" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_named_class" >:: fun ctxt ->
           let source = "export class C {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "C" named in
               match export with
               | (_, ExportNamed { loc; kind = NamedDeclaration }) ->
                 assert_substring_equal ~ctxt "C" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "export_named_vars" >:: fun ctxt ->
           let source = "export var x, y = 0, [a] = [], {p} = {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 4 (List.length named);
               let x_loc =
                 match List.assoc "x" named with
                 | (_, ExportNamed { loc; kind = NamedDeclaration }) -> loc
                 | _ -> assert_failure "Unexpected export"
               in
               let y_loc =
                 match List.assoc "y" named with
                 | (_, ExportNamed { loc; kind = NamedDeclaration }) -> loc
                 | _ -> assert_failure "Unexpected export"
               in
               let a_loc =
                 match List.assoc "a" named with
                 | (_, ExportNamed { loc; kind = NamedDeclaration }) -> loc
                 | _ -> assert_failure "Unexpected export"
               in
               let p_loc =
                 match List.assoc "p" named with
                 | (_, ExportNamed { loc; kind = NamedDeclaration }) -> loc
                 | _ -> assert_failure "Unexpected export"
               in
               assert_substring_equal ~ctxt "x" source x_loc;
               assert_substring_equal ~ctxt "y" source y_loc;
               assert_substring_equal ~ctxt "a" source a_loc;
               assert_substring_equal ~ctxt "p" source p_loc
           )
         );
         ( "export_named_specs" >:: fun ctxt ->
           let source = "export {x, y as z}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 2 (List.length named);
               let x_loc =
                 match List.assoc "x" named with
                 | ( _,
                     ExportNamed
                       { loc; kind = NamedSpecifier { local = (x_loc, "x"); source = None } }
                   )
                   when x_loc = loc ->
                   loc
                 | _ -> assert_failure "Unexpected export"
               in
               let (y_loc, z_loc) =
                 match List.assoc "z" named with
                 | ( _,
                     ExportNamed
                       { loc; kind = NamedSpecifier { local = (y_loc, "y"); source = None } }
                   ) ->
                   (y_loc, loc)
                 | _ -> assert_failure "Unexpected export"
               in
               assert_substring_equal ~ctxt "x" source x_loc;
               assert_substring_equal ~ctxt "y" source y_loc;
               assert_substring_equal ~ctxt "z" source z_loc
           )
         );
         ( "export_star" >:: fun ctxt ->
           let source = "export * from 'foo'" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_star:(function
               | [(_, ExportStar { star_loc; source = (source_loc, "foo") })] ->
                 assert_substring_equal ~ctxt "*" source star_loc;
                 assert_substring_equal ~ctxt "'foo'" source source_loc
               | _ -> assert_failure "Unexpected export"
               )
         );
         ( "export_ns" >:: fun ctxt ->
           let source = "export * as ns from 'foo'" in
           let parse_options =
             Parser_env.{ default_parse_options with esproposal_export_star_as = true }
           in
           let { module_sig = { module_kind; _ }; _ } = visit ~parse_options source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "ns" named in
               match export with
               | (_, ExportNs { loc; source = (source_loc, "foo"); _ }) ->
                 assert_substring_equal ~ctxt "ns" source loc;
                 assert_substring_equal ~ctxt "'foo'" source source_loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_module.exports" >:: fun ctxt ->
           let source = "declare module.exports: ty" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           (* TODO use just the `module.exports` location *)
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (0, 26)))
         );
         ( "declare_export_default" >:: fun ctxt ->
           let source = "declare export default string" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = None }) ->
                 assert_substring_equal ~ctxt "default" source default_loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_default_func" >:: fun ctxt ->
           let source = "declare export default function foo(): void" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = Some (loc, "foo") }) ->
                 assert_substring_equal ~ctxt "default" source default_loc;
                 assert_substring_equal ~ctxt "foo" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_default_class" >:: fun ctxt ->
           let source = "declare export default class C {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "default" named in
               match export with
               | (_, ExportDefault { default_loc; local = Some (loc, "C") }) ->
                 assert_substring_equal ~ctxt "default" source default_loc;
                 assert_substring_equal ~ctxt "C" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_named_func" >:: fun ctxt ->
           let source = "declare export function foo(): void" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "foo" named in
               match export with
               | (_, ExportNamed { loc; kind = NamedDeclaration }) ->
                 assert_substring_equal ~ctxt "foo" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_named_class" >:: fun ctxt ->
           let source = "declare export class C {}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "C" named in
               match export with
               | (_, ExportNamed { loc; kind = NamedDeclaration }) ->
                 assert_substring_equal ~ctxt "C" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_named_var" >:: fun ctxt ->
           let source = "declare export var foo: string" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               let export = assert_singleton_assoc ~ctxt "foo" named in
               match export with
               | (_, ExportNamed { loc; kind = NamedDeclaration }) ->
                 assert_substring_equal ~ctxt "foo" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_export_named_specs" >:: fun ctxt ->
           let source = "declare export {x, y as z}" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 2 (List.length named);
               let x_loc =
                 match List.assoc "x" named with
                 | ( _,
                     ExportNamed
                       { loc; kind = NamedSpecifier { local = (x_loc, "x"); source = None } }
                   )
                   when x_loc = loc ->
                   loc
                 | _ -> assert_failure "Unexpected export"
               in
               let (y_loc, z_loc) =
                 match List.assoc "z" named with
                 | ( _,
                     ExportNamed
                       { loc; kind = NamedSpecifier { local = (y_loc, "y"); source = None } }
                   ) ->
                   (y_loc, loc)
                 | _ -> assert_failure "Unexpected export"
               in
               assert_substring_equal ~ctxt "x" source x_loc;
               assert_substring_equal ~ctxt "y" source y_loc;
               assert_substring_equal ~ctxt "z" source z_loc
           )
         );
         ( "declare_export_star" >:: fun ctxt ->
           let source = "declare export * from 'foo'" in
           let { module_sig = { module_kind; _ }; _ } = visit source in
           assert_es module_kind ~assert_star:(function
               | [(_, ExportStar { star_loc; source = (source_loc, "foo") })] ->
                 assert_substring_equal ~ctxt "*" source star_loc;
                 assert_substring_equal ~ctxt "'foo'" source source_loc
               | _ -> assert_failure "Unexpected export"
               )
         );
         ( "declare_export_ns" >:: fun ctxt ->
           let source = "declare export * as ns from 'foo'" in
           let parse_options =
             Parser_env.{ default_parse_options with esproposal_export_star_as = true }
           in
           let { module_sig = { module_kind; _ }; _ } = visit ~parse_options source in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 1 (List.length named);
               let (loc, source_loc) =
                 match List.assoc "ns" named with
                 | (_, ExportNs { loc; source = (source_loc, "foo"); _ }) -> (loc, source_loc)
                 | _ -> assert_failure "Unexpected export"
               in
               assert_substring_equal ~ctxt "ns" source loc;
               assert_substring_equal ~ctxt "'foo'" source source_loc
           )
         );
         ( "declare_module" >:: fun ctxt ->
           let source = "declare module foo {}" in
           let { declare_modules; _ } = visit source in
           let modules = declare_modules in
           assert_equal ~ctxt 1 (SMap.cardinal modules);
           let (loc, { requires; module_kind; type_exports_named; type_exports_star }) =
             SMap.find "foo" modules
           in
           assert_substring_equal ~ctxt source source loc;
           assert_equal ~ctxt 0 (List.length requires);
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt None);
           assert_equal ~ctxt 0 (List.length type_exports_named);
           assert_equal ~ctxt 0 (List.length type_exports_star)
         );
         ("declare_module_export_type" >:: function
          | ctxt ->
            let source = "declare module foo { declare export type bar = string }" in
            let { declare_modules; _ } = visit source in
            assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
            let (_, { type_exports_named; _ }) = SMap.find "foo" declare_modules in
            assert_equal ~ctxt 1 (List.length type_exports_named);
            let loc =
              match List.assoc "bar" type_exports_named with
              | (_, TypeExportNamed { loc; kind = NamedDeclaration }) -> loc
              | _ -> assert_failure "Unexpected type export"
            in
            assert_substring_equal ~ctxt "bar" source loc
         );
         ( "declare_module_export_default_expr" >:: fun ctxt ->
           let source = "declare module foo { declare export default ty }" in
           let { declare_modules; _ } = visit source in
           assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
           let (_, { module_kind; _ }) = SMap.find "foo" declare_modules in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 1 (List.length named);
               match List.assoc "default" named with
               | (_, ExportDefault { default_loc; local = None }) ->
                 assert_substring_equal ~ctxt "default" source default_loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_module_export_default_decl" >:: fun ctxt ->
           let source = "declare module foo { declare export default function bar(): void }" in
           let { declare_modules; _ } = visit source in
           assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
           let (_, { module_kind; _ }) = SMap.find "foo" declare_modules in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 1 (List.length named);
               match List.assoc "default" named with
               | (_, ExportDefault { default_loc; local = Some (loc, "bar") }) ->
                 assert_substring_equal ~ctxt "default" source default_loc;
                 assert_substring_equal ~ctxt "bar" source loc
               | _ -> assert_failure "Unexpected export"
           )
         );
         ( "declare_module_export_name_func" >:: fun ctxt ->
           let source = "declare module foo { declare export function bar(): void }" in
           let { declare_modules; _ } = visit source in
           assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
           let (_, { module_kind; _ }) = SMap.find "foo" declare_modules in
           assert_es module_kind ~assert_named:(fun named ->
               assert_equal ~ctxt 1 (List.length named);
               let loc =
                 match List.assoc "bar" named with
                 | (_, ExportNamed { loc; kind = NamedDeclaration }) -> loc
                 | _ -> assert_failure "Unexpected export"
               in
               assert_substring_equal ~ctxt "bar" source loc
           )
         );
         ( "declare_module_export_star" >:: fun ctxt ->
           let source = "declare module foo { declare export * from 'bar' }" in
           let { declare_modules; _ } = visit source in
           assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
           let (_, { module_kind; _ }) = SMap.find "foo" declare_modules in
           assert_es module_kind ~assert_star:(function
               | [(_, ExportStar { star_loc; source = (source_loc, "bar") })] ->
                 assert_substring_equal ~ctxt "*" source star_loc;
                 assert_substring_equal ~ctxt "'bar'" source source_loc
               | _ -> assert_failure "Unexpected export"
               )
         );
         ( "declare_module_declare_module.export" >:: fun ctxt ->
           let source = "declare module foo { declare module.exports: ty }" in
           let { declare_modules; _ } = visit source in
           assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
           let (_, { module_kind; _ }) = SMap.find "foo" declare_modules in
           (* TODO use o0nly the location of `module.exports` *)
           assert_cjs ~source module_kind ~assert_export_loc:(assert_equal ~ctxt (Some (21, 47)))
         );
         ( "err_indeterminate_clobber_after_export" >:: fun ctxt ->
           let source = "export default 0; module.exports = 0;" in
           match visit_err source with
           | IndeterminateModuleType loc -> assert_substring_equal ~ctxt "module.exports" source loc
         );
         ( "err_indeterminate_export_after_clobber" >:: fun ctxt ->
           let source = "module.exports = 0; export default 0;" in
           match visit_err source with
           | IndeterminateModuleType loc ->
             assert_substring_equal ~ctxt "export default 0;" source loc
         );
       ]
