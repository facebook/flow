(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open OUnit2
open Require

let visit ?parse_options source =
  let ast, _ = Parser_flow.program ~parse_options source in
  program ast

let substring_loc s loc =
  let open Loc in
  let {start={offset=a; _}; _end={offset=b; _}; _} = loc in
  String.sub s a (b - a)

let assert_cjs module_kind f =
  match module_kind with
  | ES _ -> assert_failure "Expected CommonJS, got ES"
  | CommonJS { clobbered } -> f clobbered

let assert_es module_kind f =
  match module_kind with
  | CommonJS _ -> assert_failure "Expected ES, got CommonJS"
  | ES { named; batch } -> f named batch

let tests = "require" >::: [
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
    let {module_sig = {type_exports; _}; _} = visit source in
    assert_equal 1 (SMap.cardinal type_exports);
    assert_equal ~ctxt "type ty = string"
      (substring_loc source (SMap.find_unsafe "ty" type_exports))
  end;

  "export_named_opaque_type" >:: begin fun ctxt ->
    let source = "export opaque type ty = string" in
    let {module_sig = {type_exports; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports);
    assert_equal ~ctxt "opaque type ty = string"
      (substring_loc source (SMap.find_unsafe "ty" type_exports))
  end;

  "export_named_interface" >:: begin fun ctxt ->
    let source = "export interface I {}" in
    let {module_sig = {type_exports; _}; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports);
    assert_equal ~ctxt "interface I {}"
      (substring_loc source (SMap.find_unsafe "I" type_exports))
  end;

  "export_default_expr" >:: begin fun ctxt ->
    let source = "export default 0" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "0"
        (substring_loc source (SMap.find_unsafe "default" named))
    )
  end;

  "export_default_decl" >:: begin fun ctxt ->
    let source = "export default function() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function() {}"
        (substring_loc source (SMap.find_unsafe "default" named))
    )
  end;

  "export_named_func" >:: begin fun ctxt ->
    let source = "export function f() {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function f() {}"
        (substring_loc source (SMap.find_unsafe "f" named))
    )
  end;

  "export_named_class" >:: begin fun ctxt ->
    let source = "export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "class C {}"
        (substring_loc source (SMap.find_unsafe "C" named))
    )
  end;

  "export_named_vars" >:: begin fun ctxt ->
    let source = "export var x, y = 0, [a] = [], {p} = {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 4 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "x"
        (substring_loc source (SMap.find_unsafe "x" named));
      assert_equal ~ctxt "y"
        (substring_loc source (SMap.find_unsafe "y" named));
      assert_equal ~ctxt "a"
        (substring_loc source (SMap.find_unsafe "a" named));
      assert_equal ~ctxt "p"
        (substring_loc source (SMap.find_unsafe "p" named))
    )
  end;

  "export_named_specs" >:: begin fun ctxt ->
    let source = "export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "x"
        (substring_loc source (SMap.find_unsafe "x" named));
      assert_equal ~ctxt "y as z"
        (substring_loc source (SMap.find_unsafe "z" named));
    )
  end;

  "export_batch" >:: begin fun ctxt ->
    let source = "export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal batch);
      assert_equal ~ctxt "*"
        (substring_loc source (SMap.find_unsafe "foo" batch));
    )
  end;

  "export_batch_as" >:: begin fun ctxt ->
    let source = "export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "*"
        (substring_loc source (SMap.find_unsafe "ns" named));
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
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "string"
        (substring_loc source (SMap.find_unsafe "default" named));
    )
  end;

  "declare_export_default_func" >:: begin fun ctxt ->
    let source = "declare export default function f(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function f(): void"
        (substring_loc source (SMap.find_unsafe "default" named));
    )
  end;

  "declare_export_default_class" >:: begin fun ctxt ->
    let source = "declare export default class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "class C {}"
        (substring_loc source (SMap.find_unsafe "default" named));
    )
  end;

  "declare_export_named_func" >:: begin fun ctxt ->
    let source = "declare export function f(): void" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function f(): void"
        (substring_loc source (SMap.find_unsafe "f" named));
    )
  end;

  "declare_export_named_class" >:: begin fun ctxt ->
    let source = "declare export class C {}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "class C {}"
        (substring_loc source (SMap.find_unsafe "C" named));
    )
  end;

  "declare_export_named_var" >:: begin fun ctxt ->
    let source = "declare export var x: string" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "var x: string"
        (substring_loc source (SMap.find_unsafe "x" named));
    )
  end;

  "declare_export_named_specs" >:: begin fun ctxt ->
    let source = "declare export {x, y as z}" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 2 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "x"
        (substring_loc source (SMap.find_unsafe "x" named));
      assert_equal ~ctxt "y as z"
        (substring_loc source (SMap.find_unsafe "z" named));
    )
  end;

  "declare_export_batch" >:: begin fun ctxt ->
    let source = "declare export * from 'foo'" in
    let {module_sig = {module_kind; _}; _} = visit source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal batch);
      assert_equal ~ctxt "*"
        (substring_loc source (SMap.find_unsafe "foo" batch));
    )
  end;

  "declare_export_batch_as" >:: begin fun ctxt ->
    let source = "declare export * as ns from 'foo'" in
    let parse_options = Parser_env.({
      default_parse_options with
      esproposal_export_star_as = true
    }) in
    let {module_sig = {module_kind; _}; _} = visit ~parse_options source in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "*"
        (substring_loc source (SMap.find_unsafe "ns" named));
    )
  end;

  "declare_module" >:: begin fun ctxt ->
    let source = "declare module foo {}" in
    let {declare_modules; _} = visit source in
    let modules = declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal modules);
    let loc, { requires; module_kind; type_exports } =
      SMap.find_unsafe "foo" modules in
    assert_equal ~ctxt source (substring_loc source loc);
    assert_equal ~ctxt 0 (SMap.cardinal requires);
    assert_cjs module_kind (assert_equal ~ctxt None);
    assert_equal ~ctxt 0 (SMap.cardinal type_exports);
  end;

  "declare_module_export_type" >:: begin function ctxt ->
    let source = "declare module foo { declare export type ty = string }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { type_exports; _ } =
      SMap.find_unsafe "foo" declare_modules in
    assert_equal ~ctxt 1 (SMap.cardinal type_exports);
    assert_equal ~ctxt "type ty = string"
      (substring_loc source (SMap.find_unsafe "ty" type_exports));
  end;

  "declare_module_export_default_expr" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default ty }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "ty"
        (substring_loc source (SMap.find_unsafe "default" named))
    )
  end;

  "declare_module_export_default_decl" >:: begin fun ctxt ->
    let source = "declare module foo { declare export default function f(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function f(): void"
        (substring_loc source (SMap.find_unsafe "default" named))
    )
  end;
"declare_module_export_name_func" >:: begin fun ctxt ->
    let source = "declare module foo { declare export function f(): void }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 1 (SMap.cardinal named);
      assert_equal ~ctxt 0 (SMap.cardinal batch);
      assert_equal ~ctxt "function f(): void"
        (substring_loc source (SMap.find_unsafe "f" named))
    )
  end;

  "declare_module_export_batch" >:: begin fun ctxt ->
    let source = "declare module foo { declare export * from 'bar' }" in
    let {declare_modules; _} = visit source in
    assert_equal ~ctxt 1 (SMap.cardinal declare_modules);
    let _, { module_kind; _ } = SMap.find_unsafe "foo" declare_modules in
    assert_es module_kind (fun named batch ->
      assert_equal ~ctxt 0 (SMap.cardinal named);
      assert_equal ~ctxt 1 (SMap.cardinal batch);
      assert_equal ~ctxt "*"
        (substring_loc source (SMap.find_unsafe "bar" batch))
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
]
