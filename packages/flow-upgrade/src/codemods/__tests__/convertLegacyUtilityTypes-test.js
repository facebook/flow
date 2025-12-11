/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import {testCodemod} from '../../../testUtils/codemodTestUtils';
import convertLegacyUtilityTypes from '../convertLegacyUtilityTypes';

testCodemod('convertLegacyUtilityTypes', convertLegacyUtilityTypes, {
  // IGNORED
  ignored: [
    {
      description: 'variable with similar name',
      code: `const $ReadOnly = 1;`,
    },
    {
      description: 'type alias name',
      code: `type $Keys = string;`,
    },
    {
      description: '$Keys without type parameters',
      code: `type T = $Keys;`,
    },
  ],

  // TRANSFORMED
  transformed: [
    {
      description: '$NonMaybeType to NonNullable',
      code: `type T = $NonMaybeType<string | null>;`,
      output: `type T = NonNullable<string | null>;`,
    },
    {
      description: '$ReadOnly to Readonly',
      code: `type T = $ReadOnly<{x: number}>;`,
      output: `type T = Readonly<{x: number}>;`,
    },
    {
      description: '$ReadOnlyArray to ReadonlyArray',
      code: `type T = $ReadOnlyArray<string>;`,
      output: `type T = ReadonlyArray<string>;`,
    },
    {
      description: '$ReadOnlyMap to ReadonlyMap',
      code: `type T = $ReadOnlyMap<string, number>;`,
      output: `type T = ReadonlyMap<string, number>;`,
    },
    {
      description: '$ReadOnlySet to ReadonlySet',
      code: `type T = $ReadOnlySet<string>;`,
      output: `type T = ReadonlySet<string>;`,
    },
    {
      description: '$Keys to keyof',
      code: `type T = $Keys<{x: number, y: string}>;`,
      output: `type T = keyof {x: number, y: string};`,
    },
    {
      description: '$Values to Values',
      code: `type T = $Values<{x: number, y: string}>;`,
      output: `type T = Values<{x: number, y: string}>;`,
    },
    {
      description: 'mixed to unknown',
      code: `type T = mixed;`,
      output: `type T = unknown;`,
    },
    {
      description: 'multiple utility types in one file',
      code: `
type A = $ReadOnly<{x: number}>;
type B = $ReadOnlyArray<string>;
type C = $Keys<{a: 1, b: 2}>;
type D = mixed;
`,
      output: `
type A = Readonly<{x: number}>;
type B = ReadonlyArray<string>;
type C = keyof {a: 1, b: 2};
type D = unknown;
`,
    },
    {
      description: 'nested utility types',
      code: `type T = $ReadOnlyArray<$ReadOnly<{x: number}>>;`,
      output: `type T = ReadonlyArray<Readonly<{x: number}>>;`,
    },
    {
      description: 'mixed in union types',
      code: `type T = string | mixed | number;`,
      output: `type T = string | unknown | number;`,
    },
    {
      description: 'function parameter with utility type',
      code: `function foo(x: $ReadOnly<{a: string}>): mixed {}`,
      output: `function foo(x: Readonly<{a: string}>): unknown {}`,
    },
    {
      description: 'utility type with complex type parameter',
      code: `type T = $ReadOnlyArray<{x: number, y: string} | null>;`,
      output: `type T = ReadonlyArray<{x: number, y: string} | null>;`,
    },
  ],
});
