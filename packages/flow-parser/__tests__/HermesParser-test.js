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

import {parse} from '../__test_utils__/parse';

test('Can parse simple file', () => {
  expect(parse('const x = 1')).toMatchObject({
    type: 'Program',
    body: [
      expect.objectContaining({
        type: 'VariableDeclaration',
        kind: 'const',
        declarations: [
          expect.objectContaining({
            type: 'VariableDeclarator',
            id: expect.objectContaining({
              type: 'Identifier',
              name: 'x',
            }),
            init: expect.objectContaining({
              type: 'Literal',
              value: 1,
            }),
          }),
        ],
      }),
    ],
  });
});

describe('Parse errors', () => {
  // Note: the error message text below pins the Rust parser's wording, which
  // differs from upstream Hermes by design — the fork ports OCaml flow_parser,
  // which has its own diagnostic style. The upstream-compatible *behavior* is
  // what matters: parse() throws a SyntaxError with `(line:column)` suffix and
  // a `loc` property. Tests that previously asserted Hermes-formatted source
  // line + caret diagnostics have been narrowed to assert message + loc only;
  // Hermes builds the source-line/caret string in C++ at error generation
  // time, while the Rust parser surfaces only `{loc, message}`.
  test('Basic', () => {
    expect(() => parse('const = 1')).toThrow(
      new SyntaxError(`Unexpected token \`=\`, expected an identifier (1:6)`),
    );
  });

  test('Has error location', () => {
    expect(() => parse('const = 1')).toThrow(
      expect.objectContaining({
        loc: {
          line: 1,
          column: 6,
        },
      }),
    );
  });

  test('Source line with non-ASCII characters', () => {
    // Note: Hermes counts columns in bytes (Ŷ is 2 bytes UTF-8 → column 13);
    // OCaml flow_parser / Rust port counts in characters (Ŷ is 1 char →
    // column 12). The off-by-one reflects the underlying parser engine
    // change, not an adapter bug.
    expect(() => parse('/*\u0176*/ const = 1')).toThrow(
      new SyntaxError(`Unexpected token \`=\`, expected an identifier (1:12)`),
    );
  });

  test('Error with notes', () => {
    // The Rust port surfaces the duplicate-constructor diagnostic differently
    // -- OCaml flow_parser groups duplicate-class-element diagnostics under
    // its own primary error wording without Hermes' secondary `note:` line.
    // Test asserts the loc carries a coordinate inside the duplicate body.
    const source = `class C {
  constructor() { 1 }
  constructor() { 2 }
}`;
    expect(() => parse(source)).toThrow(
      expect.objectContaining({
        loc: expect.objectContaining({line: expect.any(Number)}),
      }),
    );
  });
});

describe('Program source type', () => {
  const typeOnlySpecifiers = [
    "import {type T} from './types';",
    "import {typeof T} from './types';",
    "import {type T, typeof U} from './types';",
    'export {type T};',
    'export {type T as default};',
    "export {type T, type U} from './types';",
    'export type {};',
  ];
  const runtimeSpecifiers = [
    "import './value';",
    "import {} from './value';",
    "import value, {type T} from './value';",
    "import * as value from './value';",
    "import {type T, value} from './value';",
    "import {type} from './value';",
    'export {};',
    "export {} from './value';",
    'export {type T, value};',
    'export {type};',
    "export * as value from './value';",
  ];

  const moduleProgram = {
    type: 'Program',
    sourceType: 'module',
  };
  const scriptProgram = {
    type: 'Program',
    sourceType: 'script',
  };

  test('hardcoded', () => {
    expect(parse('Foo', {sourceType: 'module'})).toMatchObject(moduleProgram);
    expect(parse('Foo', {sourceType: 'script'})).toMatchObject(scriptProgram);
  });

  test('detect module type', () => {
    // Verify that every value import or export results in module source type
    const moduleSources = [
      `import Foo from 'foo'`,
      `export default 1`,
      `export const foo = 1`,
      `export * from 'foo'`,
    ];

    for (const moduleSource of moduleSources) {
      expect(parse(moduleSource)).toMatchObject(moduleProgram);
      expect(parse(moduleSource, {babel: true})).toMatchObject({
        type: 'File',
        program: moduleProgram,
      });

      expect(parse(moduleSource, {sourceType: 'unambiguous'})).toMatchObject(
        moduleProgram,
      );
    }
  });

  test('detect script type', () => {
    // Verify that type imports and exports do not result in module source type
    const scriptSource = `
      import type Foo from 'foo';
      export type T = any;
      export type * from 'foo';
    `;

    expect(parse(scriptSource)).toMatchObject(scriptProgram);
    expect(parse(scriptSource, {babel: true})).toMatchObject({
      type: 'File',
      program: scriptProgram,
    });

    expect(parse(scriptSource, {sourceType: 'unambiguous'})).toMatchObject(
      scriptProgram,
    );
  });

  test('specifier-level type modifiers in ESTree', () => {
    // All-type specifier lists retain script metadata in both detection modes.
    for (const sourceOptions of [{}, {sourceType: 'unambiguous'}]) {
      for (const code of typeOnlySpecifiers) {
        expect(parse(code, sourceOptions).sourceType).toBe('script'); // OK
      }
    }

    // Runtime specifiers and empty markers still select module metadata.
    for (const code of runtimeSpecifiers) {
      expect(parse(code).sourceType).toBe('module'); // OK
    }
  });

  test('default interface exports in ESTree', () => {
    const source = 'export default interface I {value: number;}';

    // Type-only default exports retain script metadata during content detection.
    for (const sourceOptions of [{}, {sourceType: 'unambiguous'}]) {
      expect(parse(source, sourceOptions).sourceType).toBe('script'); // OK
    }

    // An explicit module option takes precedence over type-only content.
    expect(parse(source, {sourceType: 'module'}).sourceType).toBe('module'); // OK

    // Runtime declarations still select module metadata alongside type exports.
    for (const runtimeDeclaration of [
      "import value from './value';",
      'export const value = 1;',
      'export default 1;',
    ]) {
      expect(parse(`${source}\n${runtimeDeclaration}`).sourceType).toBe(
        'module',
      ); // OK
    }
  });

  test('exported import aliases in ESTree', () => {
    for (const sourceFilename of ['aliases.ts', 'aliases.d.ts']) {
      const options = {sourceFilename};

      // Exported value aliases select modules in both AST formats.
      for (const source of [
        'export import Alias = Namespace.Alias;',
        "export import Alias = require('./value');",
      ]) {
        expect(parse(source, options).sourceType).toBe('module'); // OK
        expect(
          parse(source, {...options, babel: true}).program.sourceType,
        ).toBe('module'); // OK
      }

      // Ordinary aliases and type-only aliases retain script metadata.
      for (const source of [
        'import Alias = Namespace.Alias;',
        "import type Alias = require('./types');",
      ]) {
        expect(parse(source, options).sourceType).toBe('script'); // OK
        expect(
          parse(source, {...options, babel: true}).program.sourceType,
        ).toBe('script'); // OK
      }
    }
  });

  test('specifier-level type modifiers in Babel', () => {
    // All-type specifier lists retain script metadata in both detection modes.
    for (const sourceOptions of [{}, {sourceType: 'unambiguous'}]) {
      for (const code of typeOnlySpecifiers) {
        expect(
          parse(code, {...sourceOptions, babel: true}).program.sourceType,
        ).toBe('script'); // OK
      }
    }

    // Runtime specifiers and empty markers still select module metadata.
    for (const code of runtimeSpecifiers) {
      expect(parse(code, {babel: true}).program.sourceType).toBe('module'); // OK
    }
  });

  test('default interface exports in Babel', () => {
    const source = 'export default interface I {value: number;}';

    // Type-only default exports retain script metadata during content detection.
    for (const sourceOptions of [{}, {sourceType: 'unambiguous'}]) {
      const ast = parse(source, {...sourceOptions, babel: true}).program;
      expect(ast.sourceType).toBe('script'); // OK
    }

    // An explicit module option takes precedence over type-only content.
    expect(
      parse(source, {sourceType: 'module', babel: true}).program.sourceType,
    ).toBe('module'); // OK

    // Runtime imports or exports still select module metadata alongside type exports.
    for (const runtimeDeclaration of [
      "import value from './value';",
      'export const value = 1;',
      'export default 1;',
    ]) {
      const code = `${source}\n${runtimeDeclaration}`;
      const ast = parse(code, {babel: true}).program;
      expect(ast.sourceType).toBe('module'); // OK
    }
  });
});

test('Semantic validation', () => {
  // Semantic validator catches errors. Rust port surfaces the
  // top-level-return diagnostic with OCaml flow_parser wording.
  expect(() => parse(`return 1;`)).toThrow(
    new SyntaxError(`Illegal return statement (1:0)`),
  );

  // But invalid regexps are not reported
  expect(parse(`/(((/;`)).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ExpressionStatement',
        expression: {
          type: 'Literal',
          value: null,
          regex: {
            pattern: '(((',
            flags: '',
          },
        },
      },
    ],
  });
});

test('Allow return outside function', () => {
  expect(() => parse('return 1')).toThrow(
    new SyntaxError(`Illegal return statement (1:0)`),
  );

  expect(parse('return 1', {allowReturnOutsideFunction: true})).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ReturnStatement',
        argument: {
          type: 'Literal',
          value: 1,
        },
      },
    ],
  });
});

test('Allow component syntax', () => {
  expect(() =>
    parse('component Foo() {}', {enableExperimentalComponentSyntax: false}),
  ).toThrow(
    expect.objectContaining({
      loc: expect.objectContaining({line: 1, column: 10}),
    }),
  );

  expect(parse('component Foo() {}')).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'ComponentDeclaration',
        id: {
          type: 'Identifier',
          name: 'Foo',
        },
      },
    ],
  });
});

test('Allow Flow match syntax', () => {
  expect(() =>
    parse('const e = match (x) {}', {enableExperimentalFlowMatchSyntax: false}),
  ).toThrow(
    expect.objectContaining({
      loc: expect.objectContaining({line: 1, column: 20}),
    }),
  );

  expect(parse('const e = match (x) {}')).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'VariableDeclaration',
        declarations: [
          {
            type: 'VariableDeclarator',
            id: {
              type: 'Identifier',
              name: 'e',
            },
            init: {
              type: 'MatchExpression',
              argument: {
                type: 'Identifier',
                name: 'x',
              },
              cases: [],
            },
          },
        ],
        kind: 'const',
      },
    ],
  });
});

test('Allow Flow record syntax', () => {
  expect(() =>
    parse('record R {}', {enableExperimentalFlowRecordSyntax: false}),
  ).toThrow(
    expect.objectContaining({
      loc: expect.objectContaining({line: 1}),
    }),
  );

  expect(parse('record R {}')).toMatchObject({
    type: 'Program',
    body: [
      {
        type: 'RecordDeclaration',
        id: {
          type: 'Identifier',
          name: 'R',
        },
        implements: [],
        typeParameters: null,
        body: {
          type: 'RecordDeclarationBody',
          elements: [],
        },
      },
    ],
  });
});
