/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// A namespace import binds a value and a namespace whose members can name
// types, so neither position may report the binding as a type used as a value.
import * as NS from './exporter';

NS.myValue satisfies number; // OK
NS.myValue satisfies string; // ERROR: number ~> string (proves the value resolved)

const ns = NS; // OK: a namespace import has a runtime value
ns.myValue satisfies number; // OK

declare const t: NS.MyType;
t satisfies number; // OK
t satisfies string; // ERROR: number ~> string (proves the type resolved)

declare const i: NS.MyInterface;
i.x satisfies string; // OK
i.x satisfies number; // ERROR: string ~> number (proves the interface resolved)

class Sub extends NS.MyClass {}
new Sub().x satisfies number; // OK
new Sub().x satisfies string; // ERROR: number ~> string (proves the class resolved)

const badType = NS.MyType; // ERROR: a type is not a runtime member of the namespace
