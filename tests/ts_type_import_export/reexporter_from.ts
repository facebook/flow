/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Test: export { T } from './exporter' — the "from" form
// T is type-only in exporter.ts, so this should be treated as a type re-export.
export { MyType } from './exporter';
export { MyInterface } from './exporter';
export { LocalType } from './exporter';
export { myValue } from './exporter';
export { MyClass } from './exporter';
export { MyEnum } from './exporter';
