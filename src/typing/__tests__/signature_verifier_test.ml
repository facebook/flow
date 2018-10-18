(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2

let parse contents =
  let parse_options = Some { Parser_env.default_parse_options with
    Parser_env.esproposal_class_instance_fields = true;
    Parser_env.esproposal_class_static_fields = true;
    Parser_env.esproposal_export_star_as = true;
  } in
  let ast, _errors = Parser_flow.program ~parse_options contents in
  ast

let eq printer v1 v2 =
  printer v1 = printer v2

let name ?prevent_munge ?ignore_static_propTypes x =
  prevent_munge, ignore_static_propTypes, x

let tests_data = [
  name "export_number_literal",
  ["export default 0;"],
  [],
  [];

  name "export_function_literal",
  ["export default function(x: number): number { return x };"],
  [],
  [];

  name "export_function_literal_check1",
  ["export default function(x): number { return x };"],
  ["Expected annotation @ (1, 24) to (1, 25)"],
  [];

  name "export_function_literal_check2",
  ["export default function(x: number) { return x };"],
  ["Expected annotation @ (1, 34) to (1, 34)"],
  [];

  name "export_function_reference",
  ["function foo(x: number): number { return x }";
   "export default foo;"],
  [],
  ["Reachable: foo"];

  name "export_function_reference_check1",
  ["function foo(x): number { return x }";
   "export default foo;"],
  ["Expected annotation @ (1, 13) to (1, 14)"],
  ["Reachable: foo"];

  name "export_function_reference_check2",
  ["function foo(x: number) { return x }";
   "export default foo;"],
  ["Expected annotation @ (1, 23) to (1, 23)"],
  ["Reachable: foo"];

  name "export_object_literal_property_literal",
  ["export default { p: 0 };"],
  [],
  [];

  name "export_object_literal_property_reference",
  ["var x: number = 0;";
   "export default { p: x };"],
  [],
  ["Reachable: x"];

  name "export_object_literal_property_reference_check",
  ["var x = 0;";
   "export default { p: x };"],
  ["Expected annotation @ (1, 4) to (1, 5)"],
  ["Reachable: x"];

  name "empty_object_literal",
  ["export default { };"],
  ["Cannot determine types of initialized properties of empty object @ (1, 15) to (1, 18)"],
  [];

  name "export_class_reference",
  ["class C {";
   "  f: number = 0;";
   "  m(x: number): number { return x; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C"];

  name "export_class_reference_check1",
  ["class C {";
   "  f = 0;";
   "  m(x: number): number { return x; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (2, 2) to (2, 8)"],
  ["Reachable: C"];

  name "export_class_reference_check2",
  ["class C {";
   "  f: number = 0;";
   "  m(x): number { return x; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (3, 4) to (3, 5)"],
  ["Reachable: C"];

  name "export_class_reference_check3",
  ["class C {";
   "  f: number = 0;";
   "  m(x: number) { return x; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (3, 14) to (3, 14)"],
  ["Reachable: C"];

  name "type_alias_dependencies",
  ["type T1 = number;";
   "type T2 = number;";
   "type T3 = number;";
   "class C {";
   "  f: T1 = 0;";
   "  m(x: T2): T3 { return x; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C, T1, T2, T3"];

  name "class_dependencies",
  ["class D { f: number = 0; }";
   "class C {";
   "  f: D = new D;";
   "  m(x: D): D { return x; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C, D"];

  name "class_dependencies_check",
  ["class D { f = 0; }";
   "class C {";
   "  f: D = new D;";
   "  m(x: D): D { return x; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (1, 10) to (1, 16)"],
  ["Reachable: C, D"];

  name "export_new_typecast",
  ["class D { f: number = 0; }";
   "class C {";
   "  f: D = new D;";
   "  m(x: D): D { return x; }";
   "}";
   "export default (new C: C);"],
  [],
  ["Reachable: C, D"];

  name "export_new_typecast_check",
  ["class D { f = 0; }";
   "class C {";
   "  f: D = new D;";
   "  m(x: D): D { return x; }";
   "}";
   "export default (new C: C);"],
  ["Expected annotation @ (1, 10) to (1, 16)"],
  ["Reachable: C, D"];

  name "recursive_dependencies",
  ["class C {";
   "  f: C = new C;";
   "  m(x: C): C { return x; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C"];

  name "recursive_dependencies_check",
  ["class C {";
   "  f = new C;";
   "  m(x: C): C { return x; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (2, 2) to (2, 12)"],
  ["Reachable: C"];

  name "typeof_dependencies",
  ["var x: number = 0";
   "class C {";
   "  p: typeof x = 0";
   "}";
   "export default (new C: C);"],
  [],
  ["Reachable: C, x"];

  name "typeof_dependencies_check",
  ["var x = 0";
   "class C {";
   "  p: typeof x = 0";
   "}";
   "export default (new C: C);"],
  ["Expected annotation @ (1, 4) to (1, 5)"],
  ["Reachable: C, x"];

  name "const_initializer",
  ["const x = 0";
   "export default { x };"],
  [],
  ["Reachable: x"];

  name "empty_array_literal",
  ["export default [ ];"],
  ["Cannot determine element type of empty array @ (1, 15) to (1, 18)"],
  [];

  name "non_empty_array_literal",
  ["const x = 0";
   "var y = false";
   "export default [ x, y ];"],
  ["Expected annotation @ (2, 4) to (2, 5)"],
  ["Reachable: x, y"];

  name "void_function",
  ["function foo() {}";
   "export default foo;"],
  [],
  ["Reachable: foo"];

  name "void_generator",
  ["function* foo() { yield 0; }";
   "export default foo;"],
  ["Expected annotation @ (1, 15) to (1, 15)"],
  ["Reachable: foo"];

  name "import_default_dependencies",
  ["import x from './import_default_dependencies_helper';";
   "class C {";
   "  p: typeof x = 0";
   "}";
   "export default (new C: C);"],
  [],
  ["import { default } from './import_default_dependencies_helper'";
   "Reachable: C, x"];

  name "import_type_dependencies",
  ["import type { T1, T2, T3 } from './import_type_dependencies_helper';";
   "class C {";
   "  f: T1 = 0;";
   "  m(x: T2): T3 { return x; }";
   "}";
   "export default C;"],
  [],
  ["import type { T1 } from './import_type_dependencies_helper'";
   "import type { T2 } from './import_type_dependencies_helper'";
   "import type { T3 } from './import_type_dependencies_helper'";
   "Reachable: C, T1, T2, T3"];

  name "qualified_references",
  ["import M1 from './qualified_references_helper';";
   "import type M2 from './qualified_references_helper';";
   "class C {";
   "  m(x: M1.T): M2.T { return x; }";
   "}";
   "export default C;"],
  [],
  ["import type { default } from './qualified_references_helper'";
   "import { default } from './qualified_references_helper'";
   "Reachable: C, M1, M2"];

  name "hoisted_requires",
  ["const M = require('./hoisted_requires_helper');";
   "if (Math.random() < 0.5) {";
   "  var { D } = require('./hoisted_requires_helper');";
   "} else {";
   "  var { D } = require('./hoisted_requires_helper');";
   "}";
   "var D = 0;";
   "class C extends M.D {";
   "  f: D = 0;";
   "}";
   "module.exports = C;"],
  ["Expected annotation @ (7, 4) to (7, 5)"],
  ["import { D } from './hoisted_requires_helper'";
   "import { D } from './hoisted_requires_helper'";
   "require('./hoisted_requires_helper')";
   "Reachable: C, D, M"];

  name "hoisted_locals",
  ["const M = require('./hoisted_locals_helper');";
   "if (Math.random() < 0.5) {";
   "  var D = 0;";
   "} else {";
   "  var D = false;";
   "}";
   "class C extends M.D {";
   "  f: D = 0;";
   "}";
   "module.exports = C;"],
  ["Unexpected toplevel definition that needs hoisting @ (3, 2) to (3, 12)";
   "Unexpected toplevel definition that needs hoisting @ (5, 2) to (5, 16)"],
  ["require('./hoisted_locals_helper')";
   "Reachable: C, D, M"];

  name "dynamic_requires",
  ["module.exports = require('./dynamic_requires_helper');"],
  [],
  ["require('./dynamic_requires_helper')"];

  name "scope_extrusion",
  ["{";
   "  class C {}";
   "  var x: C = new C;";
   "}";
   "class C {";
   "  f = 0;";
   "}";
   "module.exports = x;"],
  ["Unexpected toplevel definition that needs hoisting @ (3, 2) to (3, 19)"],
  ["Reachable: x"];

  name "scope_extrusion_nested",
  ["{";
   "  class C {}";
   "  let y = 0;";
   "  if (b) {";
   "    var x: C = new C;";
   "  }";
   "}";
   "class C {";
   "  f = 0;";
   "}";
   "module.exports = { x, y };"],
  ["Unexpected toplevel definition that needs hoisting @ (5, 4) to (5, 21)"],
  ["global value: y";
   "Reachable: x"];

  name "report_all_errors",
  ["class A {";
   "  f = (x: number) => x;     // C";
   "}";
   "module.exports = {";
   "  a: A,                     // A";
   "  b: (x: string) => x,      // B";
   "};"],
  ["Expected annotation @ (2, 2) to (2, 23)";
   "Expected annotation @ (6, 16) to (6, 16)"],
  ["Reachable: A"];

  name "munged_methods_ignored",
  ["class C {";
   "  _method() { return 1; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C"];

  name "munged_methods_not_ignored_if_directive" ~prevent_munge:true,
  ["class C {";
   "  _method() { return 1; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (2, 11) to (2, 11)"],
  ["Reachable: C"];

  name "munged_fields_ignored",
  ["class C {";
   "  _method = () => { return 1; }";
   "}";
   "export default C;"],
  [],
  ["Reachable: C"];

  name "munged_fields_not_ignored_if_directive" ~prevent_munge:true,
  ["class C {";
   "  _method = () => { return 1; }";
   "}";
   "export default C;"],
  ["Expected annotation @ (2, 2) to (2, 31)"],
  ["Reachable: C"];

  name "propTypes_static_ignored" ~ignore_static_propTypes:true,
  ["class C {";
   "  static propTypes = {}";
   "}";
   "export default C;"],
  [],
  ["Reachable: C"];

  name "propTypes_member_failure",
  ["class C {";
   "  propTypes = {}";
   "}";
   "export default C;"],
  ["Expected annotation @ (2, 2) to (2, 16)"],
  ["Reachable: C"];

  name "array_spread",
  ["module.exports = [1, ...[2, 3], 4]"],
  ["Unexpected array spread @ (1, 21) to (1, 30)"],
  [];

  name "array_hole",
  ["module.exports = [,]"],
  ["Unexpected array hole @ (1, 17) to (1, 20)"],
  [];

  name "object_spread",
  ["module.exports = { x: 'x', ...{ y: 'y' }, z: 'z' }"],
  ["Unexpected object spread @ (1, 27) to (1, 40)"],
  [];

  name "reference_expression1",
  ["module.exports = Number.NaN"],
  [],
  ["global value: Number"];

  name "reference_expression2",
  ["module.exports = 'x'.length"],
  ["Expected literal expression instead of Member @ (1, 17) to (1, 27)"],
  [];

  name "arith_expression1",
  ["module.exports = 6*7"],
  [],
  [];

  name "arith_expression2",
  ["module.exports = 6+7"],
  ["Expected literal expression instead of Binary @ (1, 17) to (1, 20)"],
  [];

  name "named_class_expression",
  ["module.exports = class C { }"],
  [],
  [];

  name "named_function_expression",
  ["module.exports = function foo() { }"],
  [],
  [];

  name "interface_coverage",
  ["declare interface Foo<X> { }";
   "declare export class C {";
   "  foo: Foo<any>;";
   "}"],
  [],
  ["Reachable: C, Foo"];

  name "bound_coverage",
  ["type Foo = number";
   "export type T = <X: Foo> (X) => void"],
  [],
  ["Reachable: Foo, T"];

]

let mk_signature_verifier_test ?prevent_munge ?ignore_static_propTypes contents expected_msgs =
  begin fun ctxt ->
    let contents = String.concat "\n" contents in
    let signature = match Signature_builder.program (parse contents) with
      | Ok signature -> signature
      | Error _ -> failwith "Signature builder failure!" in
    let errors, remote_dependencies, env =
      Signature_builder.Signature.verify ?prevent_munge ?ignore_static_propTypes signature
    in
    let error_msgs = List.map Signature_builder_deps.Error.to_string @@
      Signature_builder_deps.ErrorSet.elements errors in
    let remote_dependency_msgs = List.map Signature_builder_deps.Dep.to_string @@
      Signature_builder_deps.DepSet.elements remote_dependencies in
    let reachable_msg_opt =
      if SMap.is_empty env then []
      else [Printf.sprintf "Reachable: %s" @@ String.concat ", " @@ SMap.ordered_keys env] in
    let msgs = error_msgs @ remote_dependency_msgs @ reachable_msg_opt in
    let printer = String.concat "; " in
    assert_equal ~ctxt
      ~cmp:(eq printer)
      ~printer
      ~msg:"Results don't match!"
      expected_msgs msgs
  end

let tests = "signature_verifier" >:::
  (List.map (fun (
    (prevent_munge, ignore_static_propTypes, name),
    contents,
    error_msgs,
    other_msgs) ->
    name >:: mk_signature_verifier_test ?prevent_munge ?ignore_static_propTypes
      contents (error_msgs @ other_msgs)
   ) tests_data)
