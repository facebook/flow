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

// Type-only syntax requires types even when no annotation follows its modifiers.
describe.each([
  'abstract class C {}',
  'const C = abstract class {};',
  'abstract class C { abstract m(); }',
  'abstract class C { abstract x; }',
  'class C extends B { override m() {} }',
  'class C extends B { override x = 1; }',
  'class C { public x = 1; protected m() {} private n() {} }',
  'class C { constructor(public x) {} }',
  'class C { constructor(readonly x) {} }',
  'class C { constructor(writeonly x) {} }',
  'class C { m?(); }',
  'class C { m?(): void; }',
  'class C { m(): void; m() {} }',
  'class C { readonly x = 1; }',
  'class C { writeonly x = 1; }',
  'class C { +x = 1; }',
  'class C { -x = 1; }',
  'class C { x?; }',
  'class C { declare x; }',
  'class C extends B<number> {}',
  // Type-only imports and exports also require types.
  'export as namespace X;',
  'export {type T};',
  'export {type as};',
  'export {type as as T};',
  "import {type T} from 'foo';",
  "import {typeof X} from 'foo';",
  "import {type as} from 'foo';",
  "import {type as as T} from 'foo';",
  'export import type X = A.B;',
  'const x = value as number;',
  'const x = value satisfies number;',
  'const x = value!;',
  'const x = value?.prop!;',
  'export default interface I {}',
])('type syntax: %s', source => {
  test('types disabled', () => {
    expect(() =>
      FlowParser.parse(`${source} // ERROR`, {
        sourceFilename: 'test.js',
        enableTypes: false,
        enableEnums: true,
      }),
    ).toThrow(); // OK
  });

  // The same syntax remains accepted when types are explicitly enabled.
  test('types enabled', () => {
    expect(() =>
      FlowParser.parse(`${source} // OK`, {
        sourceFilename: 'test.js',
        enableTypes: true,
        enableEnums: true,
      }),
    ).not.toThrow(); // OK
  });
});

// Contextual keywords remain ordinary value names when they are not type modifiers.
test.each([
  'class C { abstract = 1; override = 2; public = 3; private = 4; protected = 5; readonly = 6; writeonly = 7; declare = 8; }',
  'class C { abstract() {} override() {} public() {} private() {} protected() {} readonly() {} writeonly() {} declare() {} }',
  "import {type, type as T} from 'foo';",
  'export {type, type as T};',
  // Value import aliases and export assignments do not require types.
  "import X = require('foo');",
  'import X = A.B;',
  "import type = require('foo');",
  'export import X = A.B;',
  'export import type = A.B;',
  'export = X;',
  // Enums and JavaScript line terminators keep their existing behavior.
  'enum E { A }',
  'const enum E { A }',
  'value\nas(value);',
  'value\nsatisfies(value);',
  'value\n!other;',
  // Abstract can end a JavaScript statement before a class declaration.
  'abstract\nclass C {}',
  'abstract/*\n*/class C {}',
  'export default abstract\nclass C {}',
])('value names with types disabled: %s', source => {
  expect(() =>
    FlowParser.parse(`${source} // OK`, {
      sourceFilename: 'test.js',
      enableTypes: false,
      enableEnums: true,
    }),
  ).not.toThrow(); // OK
});

// Line terminators separate ordinary JavaScript fields named like type modifiers.
describe.each([
  'abstract',
  'override',
  'public',
  'private',
  'protected',
  'readonly',
  'writeonly',
  'declare',
])('keyword field: %s', keyword => {
  test.each(['\n', '\r', '\u2028', '\u2029', '/*\n*/', '// field\n'])(
    'line terminator %j',
    separator => {
      const ast = FlowParser.parse(
        `class C { ${keyword}${separator}x; } // OK`,
        {
          sourceFilename: 'test.js',
          enableTypes: false,
        },
      );
      expect(ast.body[0].body.body).toHaveLength(2); // OK
    },
  );
});

// Type-enabled parsing continues to accept modifiers across line terminators.
test('type modifier across a line terminator', () => {
  const ast = FlowParser.parse('class C { public\nx; } // OK', {
    sourceFilename: 'test.js',
    enableTypes: true,
  });
  expect(ast.body[0].body.body).toHaveLength(1); // OK
});

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
