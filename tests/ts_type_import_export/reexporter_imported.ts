/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Test: import { T } from './exporter'; export { T }; — imported-local form
// T is type-only in exporter.ts, so re-exporting should treat it as type-only.
import { MyType, MyInterface, LocalType, myValue, MyClass, MyEnum } from './exporter';
export { MyType, MyInterface, LocalType, myValue, MyClass, MyEnum };
