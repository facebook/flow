(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open OUnit2
open Test_utils

let mk_signature_verifier_test contents expected_msgs =
  begin fun ctxt ->
    let contents = String.concat "\n" contents in
    let signature = match Signature_builder.program (parse contents) with
      | Ok signature -> signature
      | Error _ -> failwith "Signature builder failure!" in
    let errors, remote_dependencies = Signature_builder.Signature.verify signature in
    let error_msgs = List.map Signature_builder_deps.Error.to_string @@
      Signature_builder_deps.ErrorSet.elements errors in
    let remote_dependency_msgs = List.map Signature_builder_deps.Dep.to_string @@
      Signature_builder_deps.DepSet.elements remote_dependencies in
    let msgs = error_msgs @ remote_dependency_msgs in
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
    ["Expected annotation @ (1, 15) to (1, 47)"];

  "export_function_reference" >:: mk_signature_verifier_test
    ["function foo(x: number): number { return x }";
     "export default foo;"]
    [];

  "export_function_reference_check1" >:: mk_signature_verifier_test
    ["function foo(x): number { return x }";
     "export default foo;"]
    ["Expected annotation @ (1, 13) to (1, 14)"];

  "export_function_reference_check2" >:: mk_signature_verifier_test
    ["function foo(x: number) { return x }";
     "export default foo;"]
    ["Expected annotation @ (1, 9) to (1, 12)"];

  "export_object_literal_property_literal" >:: mk_signature_verifier_test
    ["export default { p: 0 };"]
    [];

  "export_object_literal_property_reference" >:: mk_signature_verifier_test
    ["var x: number = 0;";
     "export default { p: x };"]
    [];

  "export_object_literal_property_reference_check" >:: mk_signature_verifier_test
    ["var x = 0;";
     "export default { p: x };"]
    ["Expected annotation @ (1, 4) to (1, 5)"];

  "export_class_reference" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x: number): number { return x; }";
     "}";
     "export default C;"]
    [];

  "export_class_reference_check1" >:: mk_signature_verifier_test
    ["class C {";
     "  f = 0;";
     "  m(x: number): number { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 8)"];

  "export_class_reference_check2" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x): number { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (3, 4) to (3, 5)"];

  "export_class_reference_check3" >:: mk_signature_verifier_test
    ["class C {";
     "  f: number = 0;";
     "  m(x: number) { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (3, 3) to (3, 28)"];

  "type_alias_dependencies" >:: mk_signature_verifier_test
    ["type T1 = number;";
     "type T2 = number;";
     "type T3 = number;";
     "class C {";
     "  f: T1 = 0;";
     "  m(x: T2): T3 { return x; }";
     "}";
     "export default C;"]
    [];

  "class_dependencies" >:: mk_signature_verifier_test
    ["class D { f: number = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default C;"]
    [];

  "class_dependencies_check" >:: mk_signature_verifier_test
    ["class D { f = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (1, 10) to (1, 16)"];

  "export_new_typecast" >:: mk_signature_verifier_test
    ["class D { f: number = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default (new C: C);"]
    [];

  "export_new_typecast_check" >:: mk_signature_verifier_test
    ["class D { f = 0; }";
     "class C {";
     "  f: D = new D;";
     "  m(x: D): D { return x; }";
     "}";
     "export default (new C: C);"]
    ["Expected annotation @ (1, 10) to (1, 16)"];

  "recursive_dependencies" >:: mk_signature_verifier_test
    ["class C {";
     "  f: C = new C;";
     "  m(x: C): C { return x; }";
     "}";
     "export default C;"]
    [];

  "recursive_dependencies_check" >:: mk_signature_verifier_test
    ["class C {";
     "  f = new C;";
     "  m(x: C): C { return x; }";
     "}";
     "export default C;"]
    ["Expected annotation @ (2, 2) to (2, 12)"];

  "typeof_dependencies" >:: mk_signature_verifier_test
    ["var x: number = 0";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    [];

  "typeof_dependencies_check" >:: mk_signature_verifier_test
    ["var x = 0";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    ["Expected annotation @ (1, 4) to (1, 5)"];

  "import_default_dependencies" >:: mk_signature_verifier_test
    ["import x from './import_default_dependencies_helper';";
     "class C {";
     "  p: typeof x = 0";
     "}";
     "export default (new C: C);"]
    ["import { default } from './import_default_dependencies_helper'"];

  "import_type_dependencies" >:: mk_signature_verifier_test
    ["import type { T1, T2, T3 } from './import_type_dependencies_helper';";
     "class C {";
     "  f: T1 = 0;";
     "  m(x: T2): T3 { return x; }";
     "}";
     "export default C;"]
    ["import type { T1 } from './import_type_dependencies_helper'";
     "import type { T2 } from './import_type_dependencies_helper'";
     "import type { T3 } from './import_type_dependencies_helper'"];

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
     "require('./hoisted_requires_helper')"];

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
    ["Expected annotation @ (3, 6) to (3, 7)";
     "Expected annotation @ (5, 6) to (5, 7)";
     "require('./hoisted_locals_helper')"];

  "dynamic_requires" >:: mk_signature_verifier_test
    ["module.exports = require('./dynamic_requires_helper');"]
    ["require('./dynamic_requires_helper')"];
]
