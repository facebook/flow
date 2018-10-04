(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Test_utils

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

let tests = "signature_verifier" >::: [
  "export_number_literal" >:: mk_signature_verifier_test
    ["export default 0;"]
    [];

  "export_function_literal" >:: mk_signature_verifier_test
    ["export default function(x: number): number { return x };"]
    [];

  "export_function_literal_check1" >:: mk_signature_verifier_test
    ["export default function(x): number { return x };"]
    ["Expected annotation @ (1, 24) to (1, 25)"];

  "export_function_literal_check2" >:: mk_signature_verifier_test
    ["export default function(x: number) { return x };"]
    ["Expected annotation @ (1, 35) to (1, 35)"];

  "export_function_reference" >:: mk_signature_verifier_test
    ["function foo(x: number): number { return x }";
     "export default foo;"]
    ["Reachable: foo"];

  "export_function_reference_check1" >:: mk_signature_verifier_test
    ["function foo(x): number { return x }";
     "export default foo;"]
    ["Expected annotation @ (1, 13) to (1, 14)";
     "Reachable: foo"];

  "export_function_reference_check2" >:: mk_signature_verifier_test
    ["function foo(x: number) { return x }";
     "export default foo;"]
    ["Expected annotation @ (1, 24) to (1, 24)";
     "Reachable: foo"];

  "export_object_literal_property_literal" >:: mk_signature_verifier_test
    ["export default { p: 0 };"]
    [];

  "export_object_literal_property_reference" >:: mk_signature_verifier_test
    ["var x: number = 0;";
     "export default { p: x };"]
    ["Reachable: x"];

  "export_object_literal_property_reference_check" >:: mk_signature_verifier_test
    ["var x = 0;";
     "export default { p: x };"]
    ["Expected annotation @ (1, 4) to (1, 5)";
     "Reachable: x"];

  "export_class_reference" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x: number): number { return x; }";
     "}";
     "export default C;"]
    ["Reachable: C"];

  "export_class_reference_check1" >:: mk_signature_verifier_test
    ["class C {";
     "  f = 0;";
     "  m(x: number): number { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 8)";
     "Reachable: C"];

  "export_class_reference_check2" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x): number { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (3, 4) to (3, 5)";
     "Reachable: C"];

  "export_class_reference_check3" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x: number) { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (3, 15) to (3, 15)";
     "Reachable: C"];

  "type_alias_dependencies" >:: mk_signature_verifier_test
    ["type T1 = number;";
     "type T2 = number;";
     "type T3 = number;";
     "class C {";
     "  f: T1 = 0;";
     "  m(x: T2): T3 { return x; }";
     "}";
     "export default C;"]
    ["Reachable: C, T1, T2, T3"];

  "class_dependencies" >:: mk_signature_verifier_test
    ["class D { f: number = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default C;"]
    ["Reachable: C, D"];

  "class_dependencies_check" >:: mk_signature_verifier_test
    ["class D { f = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (1, 10) to (1, 16)";
     "Reachable: C, D"];

  "export_new_typecast" >:: mk_signature_verifier_test
    ["class D { f: number = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default (new C: C);"]
    ["Reachable: C, D"];

  "export_new_typecast_check" >:: mk_signature_verifier_test
    ["class D { f = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default (new C: C);"]
    ["Expected annotation @ (1, 10) to (1, 16)";
     "Reachable: C, D"];

  "recursive_dependencies" >:: mk_signature_verifier_test
    ["class C {";
     "  f: C = new C;";
     "  m(x: C): C { return x; }";
     "}";
     "export default C;"]
    ["Reachable: C"];

  "recursive_dependencies_check" >:: mk_signature_verifier_test
    ["class C {";
     "  f = new C;";
     "  m(x: C): C { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 12)";
     "Reachable: C"];

  "typeof_dependencies" >:: mk_signature_verifier_test
    ["var x: number = 0";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    ["Reachable: C, x"];

  "typeof_dependencies_check" >:: mk_signature_verifier_test
    ["var x = 0";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    ["Expected annotation @ (1, 4) to (1, 5)";
     "Reachable: C, x"];

  "const_initializer" >:: mk_signature_verifier_test
    ["const x = 0";
     "export default { x };"]
    ["Reachable: x"];

  "empty_array_literal" >:: mk_signature_verifier_test
    ["export default [ ];"]
    ["Cannot determine element type of empty array @ (1, 15) to (1, 18)"];

  "non_empty_array_literal" >:: mk_signature_verifier_test
    ["const x = 0";
     "var y = false";
     "export default [ x, y ];"]
    ["Expected annotation @ (2, 4) to (2, 5)";
     "Reachable: x, y"];

  "void_function" >:: mk_signature_verifier_test
    ["function foo() {}";
     "export default foo;"]
    ["Reachable: foo"];

  "void_generator" >:: mk_signature_verifier_test
    ["function* foo() { yield 0; }";
     "export default foo;"]
    ["Expected annotation @ (1, 16) to (1, 16)";
     "Reachable: foo"];

  "import_default_dependencies" >:: mk_signature_verifier_test
    ["import x from './import_default_dependencies_helper';";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    ["import { default } from './import_default_dependencies_helper'";
     "Reachable: C, x"];

  "import_type_dependencies" >:: mk_signature_verifier_test
    ["import type { T1, T2, T3 } from './import_type_dependencies_helper';";
     "class C {";
     "  f: T1 = 0;";
     "  m(x: T2): T3 { return x; }";
     "}";
     "export default C;"]
    ["import type { T1 } from './import_type_dependencies_helper'";
     "import type { T2 } from './import_type_dependencies_helper'";
     "import type { T3 } from './import_type_dependencies_helper'";
     "Reachable: C, T1, T2, T3"];

  "qualified_references" >:: mk_signature_verifier_test
    ["import M1 from './qualified_references_helper';";
     "import type M2 from './qualified_references_helper';";
     "class C {";
     "  m(x: M1.T): M2.T { return x; }";
     "}";
     "export default C;"]
    ["import type { default } from './qualified_references_helper'";
     "import { default } from './qualified_references_helper'";
     "Reachable: C, M1, M2"];

  "hoisted_requires" >:: mk_signature_verifier_test
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
     "module.exports = C;"]
    ["Expected annotation @ (7, 4) to (7, 5)";
     "import { D } from './hoisted_requires_helper'";
     "import { D } from './hoisted_requires_helper'";
     "require('./hoisted_requires_helper')";
     "Reachable: C, D, M"];

  "hoisted_locals" >:: mk_signature_verifier_test
    ["const M = require('./hoisted_locals_helper');";
     "if (Math.random() < 0.5) {";
     "  var D = 0;";
     "} else {";
     "  var D = false;";
     "}";
     "class C extends M.D {";
     "  f: D = 0;";
     "}";
     "module.exports = C;"]
    ["Unexpected toplevel definition that needs hoisting @ (3, 6) to (3, 7)";
     "Unexpected toplevel definition that needs hoisting @ (5, 6) to (5, 7)";
     "require('./hoisted_locals_helper')";
     "Reachable: C, D, M"];

  "dynamic_requires" >:: mk_signature_verifier_test
    ["module.exports = require('./dynamic_requires_helper');"]
    ["require('./dynamic_requires_helper')"];

  "scope_extrusion" >:: mk_signature_verifier_test
    ["{";
     "  class C {}";
     "  var x: C = new C;";
     "}";
     "class C {";
     "  f = 0;";
     "}";
     "module.exports = x;"]
    ["Unexpected toplevel definition that needs hoisting @ (3, 6) to (3, 7)";
     "Reachable: x"];

  "scope_extrusion_nested" >:: mk_signature_verifier_test
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
     "module.exports = { x, y };"]
    ["Unexpected toplevel definition that needs hoisting @ (5, 8) to (5, 9)";
     "global value: y";
     "Reachable: x"];

  "report_all_errors" >:: mk_signature_verifier_test
    ["class A {";
     "  f = (x: number) => x;     // C";
     "}";
     "module.exports = {";
     "  a: A,                     // A";
     "  b: (x: string) => x,      // B";
     "};"]
    ["Expected annotation @ (2, 2) to (2, 23)";
     "Expected annotation @ (6, 17) to (6, 17)";
     "Reachable: A"];

  "munged_methods_ignored" >:: mk_signature_verifier_test
    ["class C {";
     "  _method() { return 1; }";
     "}";
     "export default C;"]
    ["Reachable: C"];

  "munged_methods_not_ignored_if_directive" >:: mk_signature_verifier_test
    ~prevent_munge:true
    ["class C {";
     "  _method() { return 1; }";
     "}";
    "export default C;"]
    ["Expected annotation @ (2, 12) to (2, 12)";
     "Reachable: C"];

  "munged_fields_ignored" >:: mk_signature_verifier_test
    ["class C {";
     "  _method = () => { return 1; }";
     "}";
     "export default C;"]
    ["Reachable: C"];

  "munged_fields_not_ignored_if_directive" >:: mk_signature_verifier_test
    ~prevent_munge:true
    ["class C {";
     "  _method = () => { return 1; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 31)";
     "Reachable: C"];

  "propTypes_static_ignored" >:: mk_signature_verifier_test
    ~ignore_static_propTypes:true
    ["class C {";
     "  static propTypes = {}";
     "}";
    "export default C;"]
    ["Reachable: C"];

  "propTypes_member_failure" >:: mk_signature_verifier_test
    ["class C {";
     "  propTypes = {}";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 16)";
     "Reachable: C"];

  "array_spread" >:: mk_signature_verifier_test
    ["module.exports = [1, ...[2, 3], 4]"]
    ["Unexpected array spread @ (1, 21) to (1, 30)"];

  "object_spread" >:: mk_signature_verifier_test
    ["module.exports = { x: 'x', ...{ y: 'y' }, z: 'z' }"]
    ["Unexpected object spread @ (1, 27) to (1, 40)"];

  "reference_expression1" >:: mk_signature_verifier_test
    ["module.exports = Number.NaN"]
    ["global value: Number"];

  "reference_expression2" >:: mk_signature_verifier_test
    ["module.exports = 'x'.length"]
    ["Expected literal expression instead of Member @ (1, 17) to (1, 27)"];

  "arith_expression1" >:: mk_signature_verifier_test
    ["module.exports = 6*7"]
    [];

  "arith_expression2" >:: mk_signature_verifier_test
    ["module.exports = 6+7"]
    ["Expected literal expression instead of Binary @ (1, 17) to (1, 20)"];

  "named_class_expression" >:: mk_signature_verifier_test
    ["module.exports = class C { }"]
    [];

  "named_function_expression" >:: mk_signature_verifier_test
    ["module.exports = function foo() { }"]
    [];

]
