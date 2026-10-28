/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Disabled assertions remain unsupported within nested and computed member chains.
function members(value: ?{method: () => ?{prop: ?number}}): void {
  value!.method()!.prop!; // ERROR
  value!.method()!['prop']; // ERROR
  value?.method()!['prop']!; // ERROR
  (value?.method())!['prop']; // ERROR
}

// Optional calls and parenthesized chains do not bypass the assertion type gate.
function calls(value: ?(() => {child: ?{prop: number}})): void {
  value!(); // ERROR
  value?.().child!.prop; // ERROR
  (value?.().child)!.prop; // ERROR
}

// Assertions on private member receivers require the same opt-in as public members.
class PrivateMember {
  #prop: number = 42;

  read(value: ?PrivateMember): void {
    value!.#prop; // ERROR
  }
}

// Delete-property continuations report the assertion span, not the following property.
function deleteProperty(value: ?{child: ?{prop?: number}}): void {
  delete value?.child!.prop; // ERROR
}
