/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Test importing from re-exporters — both forms should work identically

// From the "export { T } from './exporter'" re-exporter
import { MyType as FromType, MyInterface as FromIface, LocalType as FromLocal } from './reexporter_from';
declare const a: FromType;
a satisfies number; // OK
a satisfies string; // ERROR: number ~> string
declare const b: FromIface;
b.x satisfies string; // OK
b.x satisfies number; // ERROR: string ~> number
const bad1 = FromType; // ERROR: type used as value
const bad2 = FromIface; // ERROR: type used as value
const bad3 = FromLocal; // ERROR: type used as value

// Value re-exported through "from" form should remain usable as a value
import { myValue as fromValue, MyClass as FromClass, MyEnum as FromEnum } from './reexporter_from';
fromValue satisfies number; // OK
fromValue satisfies string; // ERROR: number ~> string

// Class re-exported through "from" form should remain constructible
const fromObj = new FromClass(); // OK: class is a value
fromObj.x satisfies number; // OK

// Enum re-exported through "from" form should remain usable as a value
FromEnum.A satisfies string; // OK

// From the "import then export" re-exporter
import { MyType as ImpType, MyInterface as ImpIface, LocalType as ImpLocal, myValue as impValue, MyClass as ImpClass, MyEnum as ImpEnum } from './reexporter_imported';
declare const c: ImpType;
c satisfies number; // OK
c satisfies string; // ERROR: number ~> string
declare const d: ImpIface;
d.x satisfies string; // OK
d.x satisfies number; // ERROR: string ~> number
const bad4 = ImpType; // ERROR: type used as value
const bad5 = ImpIface; // ERROR: type used as value
const bad6 = ImpLocal; // ERROR: type used as value

// Value re-exported through "import then export" should remain usable as a value
impValue satisfies number; // OK
impValue satisfies string; // ERROR: number ~> string

// Class re-exported through "import then export" should remain constructible
const impObj = new ImpClass(); // OK: class is a value
impObj.x satisfies number; // OK

// Enum re-exported through "import then export" should remain usable as a value
ImpEnum.A satisfies string; // OK
