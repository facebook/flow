/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+flow
 * @format
 */

'use strict';

const runTest = require('../../run-test');

runTest('use-indexed-access-type', {
  valid: [
    `type T = Foo[Key];`,
    `type T = Foo?.[Key];`,
    `const x = 1;`,
    `type T = Array<string>;`,
  ],
  invalid: [
    // ElementType
    {
      code: `type T = $ElementType<Foo, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'Foo',
            index: 'Key',
          },
        },
      ],
      output: `type T = Foo[Key];`,
    },
    {
      code: `type T = $ElementType<Foo, 0>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'Foo',
            index: '0',
          },
        },
      ],
      output: `type T = Foo[0];`,
    },
    {
      code: `type T = $ElementType<Foo, number>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'Foo',
            index: 'number',
          },
        },
      ],
      output: `type T = Foo[number];`,
    },
    {
      code: `type T = $ElementType<Foo, string>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'Foo',
            index: 'string',
          },
        },
      ],
      output: `type T = Foo[string];`,
    },
    {
      code: `type T = $ElementType<ComplexType<1, 2>, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'ComplexType<1, 2>',
            index: 'Key',
          },
        },
      ],
      output: `type T = ComplexType<1, 2>[Key];`,
    },
    {
      code: `type T = $ElementType<Foo, ComplexType<1, 2>>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: 'Foo',
            index: 'ComplexType<1, 2>',
          },
        },
      ],
      output: `type T = Foo[ComplexType<1, 2>];`,
    },
    {
      code: `type T = $ElementType<
  X | Y,
  ComplexType<1, 2>,
>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(X | Y)',
            index: 'ComplexType<1, 2>',
          },
        },
      ],
      output: `type T = (X | Y)[ComplexType<1, 2>];`,
    },
    {
      code: `type T = $ElementType<
  // $FlowFixMe
  X | Y,
  ComplexType<1, 2>,
>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(X | Y)',
            index: 'ComplexType<1, 2>',
          },
        },
      ],
      output: null,
    },
    // PropertyType
    {
      code: `type T = $PropertyType<Foo, 'k'>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Foo',
            index: "'k'",
          },
        },
      ],
      output: `type T = Foo['k'];`,
    },
    {
      code: `type T = $PropertyType<Foo, ComplexType<1, 2>>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Foo',
            index: 'ComplexType<1, 2>',
          },
        },
      ],
      output: `type T = Foo[ComplexType<1, 2>];`,
    },
    // Add parens when needed
    {
      code: `type T = $ElementType<A | B, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(A | B)',
            index: 'Key',
          },
        },
      ],
      output: `type T = (A | B)[Key];`,
    },
    {
      code: `type T = $ElementType<A & B, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(A & B)',
            index: 'Key',
          },
        },
      ],
      output: `type T = (A & B)[Key];`,
    },
    {
      code: `type T = $ElementType<typeof A, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(typeof A)',
            index: 'Key',
          },
        },
      ],
      output: `type T = (typeof A)[Key];`,
    },
    {
      code: `type T = $ElementType<?A, Key>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$ElementType',
            object: '(?A)',
            index: 'Key',
          },
        },
      ],
      output: `type T = (?A)[Key];`,
    },
    // On malformed `$PropertyType`, we use example object/index
    {
      code: `type T = $PropertyType;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Obj',
            index: "'prop'",
          },
        },
      ],
      output: null,
    },
    {
      code: `type T = $PropertyType<>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Obj',
            index: "'prop'",
          },
        },
      ],
      output: null,
    },
    {
      code: `type T = $PropertyType<A>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Obj',
            index: "'prop'",
          },
        },
      ],
      output: null,
    },
    {
      code: `type T = $PropertyType<A, B, C>;`,
      errors: [
        {
          messageId: 'useIndexedAccess',
          data: {
            util: '$PropertyType',
            object: 'Obj',
            index: "'prop'",
          },
        },
      ],
      output: null,
    },
  ],
});
