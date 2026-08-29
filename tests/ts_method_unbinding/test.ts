/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class A {
  m(): void {}
  n = (): void => {}
}

interface I {
  m(): void;
  n: () => void;
}

declare const a: A;
declare const i: I;

// Extracting class methods is allowed and retains the receiver type. Because
// the receiver is retained, the extracted method is not a `() => void`: that
// annotation has an implicit `this: unknown`, which is not a valid `A`.
a.m; // OK
a.m satisfies () => void; // ERROR: `A` receiver is not satisfied by `unknown`
a.m satisfies empty; // ERROR: proves type is not any

const {m} = a; // OK
m satisfies () => void; // ERROR: `A` receiver is not satisfied by `unknown`
m satisfies empty; // ERROR: proves type is not any

if (a.m) {} // OK

// Extracting interface methods preserves their receiver too.
i.m; // OK
i.m satisfies () => void; // ERROR: `I` receiver is not satisfied by `unknown`
i.m satisfies empty; // ERROR: proves type is not any

const {m: im} = i; // OK
im satisfies () => void; // ERROR: `I` receiver is not satisfied by `unknown`
im satisfies empty; // ERROR: proves type is not any

// The original `this` type is preserved.
class B {
  self(): this { return this; }
}
declare const b: B;
const {self} = b; // OK
self satisfies (this: B) => B; // OK
self satisfies (this: string) => B; // ERROR: the receiver type is still B
self satisfies empty; // ERROR: function type is not empty

// Calling an extracted method without its receiver is rejected.
const f = a.m;
f(); // ERROR

const {m: am} = a;
am(); // ERROR

// Arrow function properties are always OK (unchanged behavior)
a.n; // OK
i.n; // OK

// Calling methods is always OK (unchanged behavior)
a.m(); // OK
i.m(); // OK

class PrivateMethod {
  #method(): void {}

  unbind(): void {
    const method = this.#method; // OK in .ts
    method(); // OK
  }
}

import {FlowClass} from "./flow_lib";

declare const flowImported: FlowClass;
flowImported.method; // OK: the consumer is .ts
