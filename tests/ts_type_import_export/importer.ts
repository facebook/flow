/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import {MyType} from './exporter';
import {MyInterface} from './exporter';
import {MyNamespace} from './exporter';
declare const x: MyType;
x as number; // OK
x as string; // ERROR: number ~> string (proves the type was resolved)
declare const y: MyInterface;
y.x as string; // OK
y.x as number; // ERROR: string ~> number (proves the interface resolved)
const z = MyType; // ERROR: type used as value
const w = MyInterface; // ERROR: type used as value
const ns = MyNamespace; // ERROR: type used as value

// Test local type bindings exported without `type` keyword
import { LocalType, LocalInterface } from './exporter';
declare const lt: LocalType;
lt as string; // OK
lt as number; // ERROR

declare const li: LocalInterface;
li.y as number; // OK

const badLocalTypeValue = LocalType; // ERROR
const badLocalInterfaceValue = LocalInterface; // ERROR
