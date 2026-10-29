/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import {parse} from '../oxidized-src/index';
import * as FlowParser from '../oxidized-src/FlowParser';

// TypeScript filenames enable annotations and ambiguous types without a Flow pragma.
describe.each([
  'test.ts',
  'test.tsx',
  'test.cts',
  'test.mts',
  'test.d.ts',
  'test.d.cts',
  'test.d.mts',
])('TypeScript types in %s', sourceFilename => {
  test.each(['f<number>(value);', 'new C<number>(value);'])(
    'type arguments in %s',
    source => {
      const code = `const value: number = 1; // OK\n${source} // OK`;
      const expected = parse(code, {sourceFilename, flow: 'all'});
      expect(parse(code, {sourceFilename, flow: 'detect'})).toEqual(expected); // OK
      expect(
        FlowParser.parse(code, {sourceFilename, enableTypes: false}),
      ).toMatchObject({body: expected.body}); // OK
    },
  );
});

// JavaScript and unnamed sources continue to respect disabled type options.
describe.each([undefined, 'test.js', 'test.ts.js'])(
  'type options in %s',
  sourceFilename => {
    test('disabled annotations', () => {
      expect(() =>
        FlowParser.parse('const value: number = 1; // ERROR', {
          sourceFilename,
          enableTypes: false,
        }),
      ).toThrow(); // OK
    });

    // Detect mode keeps ambiguous expressions as JavaScript comparisons.
    test('comparison without a Flow pragma', () => {
      expect(
        parse('f<number>(value); // OK', {sourceFilename, flow: 'detect'})
          .body[0],
      ).toMatchObject({
        type: 'ExpressionStatement',
        expression: {type: 'BinaryExpression', operator: '>'},
      }); // OK
    });
  },
);
