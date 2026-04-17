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
a as number; // OK
a as string; // ERROR: number ~> string
declare const b: FromIface;
b.x as string; // OK
b.x as number; // ERROR: string ~> number
const bad1 = FromType; // ERROR: type used as value
const bad2 = FromIface; // ERROR: type used as value
const bad3 = FromLocal; // ERROR: type used as value

// Value re-exported through "from" form should remain usable as a value
import { myValue as fromValue, MyClass as FromClass, MyEnum as FromEnum } from './reexporter_from';
fromValue as number; // OK
fromValue as string; // ERROR: number ~> string

// Class re-exported through "from" form should remain constructible
const fromObj = new FromClass(); // OK: class is a value
fromObj.x as number; // OK

// Enum re-exported through "from" form should remain usable as a value
FromEnum.A as string; // OK

// From the "import then export" re-exporter
import { MyType as ImpType, MyInterface as ImpIface, LocalType as ImpLocal, myValue as impValue, MyClass as ImpClass, MyEnum as ImpEnum } from './reexporter_imported';
declare const c: ImpType;
c as number; // OK
c as string; // ERROR: number ~> string
declare const d: ImpIface;
d.x as string; // OK
d.x as number; // ERROR: string ~> number
const bad4 = ImpType; // ERROR: type used as value
const bad5 = ImpIface; // ERROR: type used as value
const bad6 = ImpLocal; // ERROR: type used as value

// Value re-exported through "import then export" should remain usable as a value
impValue as number; // OK
impValue as string; // ERROR: number ~> string

// Class re-exported through "import then export" should remain constructible
const impObj = new ImpClass(); // OK: class is a value
impObj.x as number; // OK

// Enum re-exported through "import then export" should remain usable as a value
ImpEnum.A as string; // OK
