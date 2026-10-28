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

import type {ESNode} from 'flow-estree';

import {
  isAbstractMethodDefinition,
  isAbstractPropertyDefinition,
  isClassMember,
  isConstructorTypeAnnotation,
  isDeclareClassExtendsCall,
  isDeclareMethodDefinition,
  isExportAssignment,
  isExportNamespaceSpecifier,
  isExpression,
  isExternalModuleReference,
  isImportEqualsDeclaration,
  isImportType,
  isNamespaceExportDeclaration,
  isNonNullExpression,
  isObjectTypePrivateField,
  isParameterProperty,
  isSatisfiesExpression,
  isTemplateLiteralTypeAnnotation,
  isTupleTypeElement,
} from 'flow-estree';
import {parse, printForSnapshotBabel} from '../__test_utils__/parse';
import {SimpleTraverser} from '../oxidized-src/traverse/SimpleTraverser';

describe('TypeScript-compatible syntax', () => {
  test('template literal type annotations', () => {
    const [template] = parse(
      `type Template = \`${'${"a" | "b"}'}-${'${"x" | "y"}'}\`;`,
    ).body;
    if (template.type !== 'TypeAlias') {
      throw new Error('expected a type alias');
    }

    expect(isTemplateLiteralTypeAnnotation(template.right)).toBe(true);
  });

  test('abstract property definitions', () => {
    const [base] = parse(
      'abstract class Base { abstract value: number; }',
    ).body;
    if (base.type !== 'ClassDeclaration') {
      throw new Error('expected an abstract class declaration');
    }

    expect(isAbstractPropertyDefinition(base.body.body[0])).toBe(true);
  });

  test('abstract method definitions', () => {
    const [base] = parse(
      'abstract class Base { abstract method(x: number): string; }',
    ).body;
    if (base.type !== 'ClassDeclaration') {
      throw new Error('expected an abstract class declaration');
    }

    expect(isAbstractMethodDefinition(base.body.body[0])).toBe(true);
  });

  test('semicolonless abstract method definitions', () => {
    const classes = parse(`
      abstract class BeforeClosingBrace {
        abstract method(): void
      }

      abstract class BeforeNextMember {
        abstract first(): void
        abstract second(): void;
      }

      abstract class BeforeLineComment {
        abstract first(): void // comment
        abstract second(): void;
      }
    `).body;

    expect(classes).toHaveLength(3);
    for (const class_ of classes) {
      if (class_.type !== 'ClassDeclaration') {
        throw new Error('expected an abstract class declaration');
      }
      expect(class_.body.body.every(isAbstractMethodDefinition)).toBe(true);
    }
  });

  test('abstract methods with a body remain invalid', () => {
    for (const method of [
      'abstract method(): void {}',
      'abstract method(): void\n{}',
    ]) {
      expect(() => parse(`abstract class Base { ${method} }`)).toThrow(
        'Abstract methods cannot have an implementation.',
      );
    }
  });

  test('same-line abstract methods require a semicolon', () => {
    expect(() =>
      parse(`
        abstract class Base {
          abstract first(): void abstract second(): void;
        }
      `),
    ).toThrow('Abstract methods cannot have an implementation.');
  });

  test('override property definitions', () => {
    const [derived] = parse(
      'class Derived extends Base { override value: number; }',
    ).body;
    if (derived.type !== 'ClassDeclaration') {
      throw new Error('expected a class declaration');
    }

    expect(derived.body.body[0]).toMatchObject({
      type: 'PropertyDefinition',
      override: true,
    });
  });

  test('override method definitions', () => {
    const [derived] = parse(
      'class Derived extends Base { override method(): string { return ""; } }',
    ).body;
    if (derived.type !== 'ClassDeclaration') {
      throw new Error('expected a class declaration');
    }

    expect(derived.body.body[0]).toMatchObject({
      type: 'MethodDefinition',
      override: true,
    });
  });

  test('constructor type annotations', () => {
    const [constructor] = parse(
      'type Constructor = new (x: number) => Result;',
    ).body;
    if (constructor.type !== 'TypeAlias') {
      throw new Error('expected a constructor type alias');
    }

    expect(isConstructorTypeAnnotation(constructor.right)).toBe(true);
  });

  test('abstract constructor type annotations', () => {
    const [constructor] = parse(
      'type Constructor = abstract new (x: number) => Result;',
    ).body;
    if (constructor.type !== 'TypeAlias') {
      throw new Error('expected an abstract constructor type alias');
    }

    expect(constructor.right).toMatchObject({
      type: 'ConstructorTypeAnnotation',
      abstract: true,
    });
  });

  test('satisfies expressions', () => {
    const [value] = parse('const value = expression satisfies SomeType;').body;
    if (value.type !== 'VariableDeclaration') {
      throw new Error('expected a variable declaration');
    }

    const initializer = value.declarations[0].init;
    expect(initializer != null && isSatisfiesExpression(initializer)).toBe(
      true,
    );
  });

  test('import types', () => {
    const [imported] = parse(
      'type Imported = import("./module").Exported;',
    ).body;
    if (imported.type !== 'TypeAlias') {
      throw new Error('expected an import type alias');
    }
    expect(imported.right).toMatchObject({
      type: 'GenericTypeAnnotation',
      id: {
        type: 'QualifiedTypeIdentifier',
        qualification: {
          type: 'ImportType',
          source: {type: 'Literal', value: './module'},
        },
      },
    });
    if (
      imported.right.type !== 'GenericTypeAnnotation' ||
      imported.right.id.type !== 'QualifiedTypeIdentifier'
    ) {
      throw new Error('expected a qualified import type');
    }

    expect(isImportType(imported.right.id.qualification)).toBe(true);
  });

  test('import-equals declarations', () => {
    const [importEquals] = parse('import Module = require("module");').body;
    expect(isImportEqualsDeclaration(importEquals)).toBe(true);
    if (!isImportEqualsDeclaration(importEquals)) {
      throw new Error('expected an import-equals declaration');
    }

    expect(isExternalModuleReference(importEquals.moduleReference)).toBe(true);
  });

  test('export assignments', () => {
    const [assignment] = parse('export = Module;').body;

    expect(isExportAssignment(assignment)).toBe(true);
  });

  test('unique symbol types', () => {
    const [unique] = parse('type Unique = unique symbol;').body;

    expect(unique).toMatchObject({
      type: 'TypeAlias',
      right: {
        type: 'TypeOperator',
        operator: 'unique',
        typeAnnotation: {type: 'SymbolTypeAnnotation'},
      },
    });
  });

  test('declare-class extends calls', () => {
    const [ambient] = parse(
      'declare class Ambient extends Mixin(Base) {}',
    ).body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(isDeclareClassExtendsCall(ambient.extends[0])).toBe(true);
  });

  test('private declare-class fields', () => {
    const [ambient] = parse('declare class Ambient { #private; }').body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(isObjectTypePrivateField(ambient.body.properties[0])).toBe(true);
  });

  test('abstract declare-class fields', () => {
    const [ambient] = parse(
      'declare class Ambient { protected abstract field: string; }',
    ).body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(ambient.body.properties[0]).toMatchObject({
      type: 'ObjectTypeProperty',
      abstract: true,
      tsAccessibility: 'protected',
    });
  });

  test('override declare-class methods', () => {
    const [ambient] = parse(
      'declare class Ambient { public override method(): void; }',
    ).body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(ambient.body.properties[0]).toMatchObject({
      type: 'ObjectTypeProperty',
      override: true,
      tsAccessibility: 'public',
    });
  });

  test('initialized declare-class fields', () => {
    const [ambient] = parse(
      'declare class Ambient { readonly literal = "value"; }',
    ).body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(ambient.body.properties[0]).toMatchObject({
      type: 'ObjectTypeProperty',
      init: {type: 'Literal', value: 'value'},
    });
  });

  test('optional declare-class indexers', () => {
    const [ambient] = parse(
      'declare class Ambient { [key: string]?: number; }',
    ).body;
    if (ambient.type !== 'DeclareClass') {
      throw new Error('expected a declare class');
    }

    expect(ambient.body.indexers[0]).toMatchObject({
      type: 'ObjectTypeIndexer',
      optional: true,
    });
  });

  test('class indexers are class members', () => {
    const [indexed] = parse('export class Indexed {[key: string]: number;}', {
      sourceFilename: 'definition.d.ts',
    }).body;
    if (indexed.type !== 'ExportNamedDeclaration') {
      throw new Error('expected an exported class declaration');
    }
    const indexedDeclaration = indexed.declaration;
    if (indexedDeclaration?.type !== 'ClassDeclaration') {
      throw new Error('expected an exported class declaration');
    }

    const indexer = indexedDeclaration.body.body[0];
    expect(indexer).toMatchObject({type: 'ObjectTypeIndexer'});
    expect(isClassMember(indexer)).toBe(true);
  });

  test('optional tuple elements', () => {
    const [tuple] = parse('type Tuple = [string?];').body;
    if (tuple.type !== 'TypeAlias') {
      throw new Error('expected a type alias');
    }
    const tupleType = tuple.right;
    if (tupleType.type !== 'TupleTypeAnnotation') {
      throw new Error('expected a tuple type alias');
    }

    expect(isTupleTypeElement(tupleType.elementTypes[0])).toBe(true);
    expect(tupleType.elementTypes[0]).toMatchObject({optional: true});
  });

  test('inline type exports', () => {
    const [typeExport] = parse('export {type Tuple};').body;
    if (
      typeExport.type !== 'ExportNamedDeclaration' ||
      typeExport.declaration != null
    ) {
      throw new Error('expected a named export');
    }

    expect(typeExport).toMatchObject({specifiers: [{exportKind: 'type'}]});
  });

  test('namespace export declarations', () => {
    const [namespaceExport] = parse('export as namespace Foo;').body;

    expect(isNamespaceExportDeclaration(namespaceExport)).toBe(true);
  });

  test('anonymous ambient functions', () => {
    const [anonymous] = parse('export default function <T>(x: T): T;', {
      sourceFilename: 'definition.js.flow',
    }).body;
    if (
      anonymous.type !== 'DeclareExportDeclaration' ||
      anonymous.declaration == null ||
      anonymous.declaration.type !== 'DeclareFunction'
    ) {
      throw new Error('expected an anonymous declare function export');
    }

    expect(anonymous.declaration).toMatchObject({
      id: null,
      typeAnnotation: {
        type: 'TypeAnnotation',
        typeAnnotation: {type: 'FunctionTypeAnnotation'},
      },
    });
  });

  test('exported namespaces', () => {
    const [namespace] = parse(
      'export module Protocol { export type ID = string; }',
      {sourceFilename: 'definition.d.ts'},
    ).body;
    if (namespace.type !== 'DeclareExportDeclaration') {
      throw new Error('expected a declare export');
    }

    expect(namespace.declaration).toMatchObject({type: 'DeclareNamespace'});
  });

  test('non-null expressions', () => {
    // Non-null assertions expose the same expression child as TSNonNullExpression.
    const ast = parse('value!;');
    const [statement] = ast.body;
    if (statement.type !== 'ExpressionStatement') {
      throw new Error('expected an expression statement');
    }

    expect(isNonNullExpression(statement.expression)).toBe(true); // OK
    expect(isExpression(statement.expression)).toBe(true); // OK
    expect(statement.expression).toMatchObject({
      type: 'NonNullExpression',
      range: [0, 6],
      expression: {type: 'Identifier', name: 'value', range: [0, 5]},
    }); // OK
    expect(statement.expression).not.toHaveProperty('argument'); // OK
    expect(statement.expression).not.toHaveProperty('chain'); // OK
  });

  test('parameter properties', () => {
    const [declaration] = parse(
      'class C { constructor(public value: number) {} }',
      {sourceFilename: 'definition.d.ts'},
    ).body;
    if (declaration.type !== 'ClassDeclaration') {
      throw new Error('expected a class declaration');
    }

    const constructor = declaration.body.body[0];
    if (constructor.type !== 'MethodDefinition') {
      throw new Error('expected a method definition');
    }
    expect(isParameterProperty(constructor.value.params[0])).toBe(true);
  });

  test('declare method definitions', () => {
    const [declaration] = parse('class C { method(): void; }', {
      sourceFilename: 'definition.d.ts',
    }).body;
    if (declaration.type !== 'ClassDeclaration') {
      throw new Error('expected a class declaration');
    }

    const method = declaration.body.body[0];
    expect(isDeclareMethodDefinition(method)).toBe(true);
    expect(isClassMember(method)).toBe(true);
  });

  test.each([
    'definition.ts',
    'definition.tsx',
    'definition.mts',
    'definition.cts',
  ])('overload signatures retain accessibility in %s', sourceFilename => {
    for (const accessibility of ['private', 'protected', 'public']) {
      const [declaration] = parse(
        `class C {
            ${accessibility} m(x: number): void;
            ${accessibility} m(x: string): void;
            ${accessibility} m(x: number | string) {}
          }`,
        {sourceFilename},
      ).body;
      if (declaration.type !== 'ClassDeclaration') {
        throw new Error('expected a class declaration');
      }
      expect(declaration.body.body).toMatchObject([
        {type: 'DeclareMethodDefinition', tsAccessibility: accessibility},
        {type: 'DeclareMethodDefinition', tsAccessibility: accessibility},
        {type: 'MethodDefinition', tsAccessibility: accessibility},
      ]);
    }
  });

  test('exported ambient methods retain accessibility and other modifiers', () => {
    const [declaration] = parse(
      `export class C extends Base {
        private constructor();
        private m(x: number): void;
        protected n(x: string): void;
        public p(): void;
        protected override optional?<T>(x: T): T;
        public static [Symbol.iterator](): Iterator<number>;
        private get value(): number;
        private set value(x: number);
        protected get other(): string;
        protected set other(x: string);
        public get name(): string;
        public set name(x: string);
        unmodified(): void
      }`,
      {sourceFilename: 'definition.d.ts'},
    ).body;
    if (
      declaration.type !== 'ExportNamedDeclaration' ||
      declaration.declaration?.type !== 'ClassDeclaration'
    ) {
      throw new Error('expected an exported class declaration');
    }
    const methods = declaration.declaration.body.body;
    expect(methods.every(isDeclareMethodDefinition)).toBe(true);
    expect(methods).toMatchObject([
      {tsAccessibility: 'private'},
      {tsAccessibility: 'private'},
      {tsAccessibility: 'protected'},
      {tsAccessibility: 'public'},
      {tsAccessibility: 'protected', optional: true, override: true},
      {tsAccessibility: 'public', static: true, computed: true},
      {tsAccessibility: 'private', kind: 'get'},
      {tsAccessibility: 'private', kind: 'set'},
      {tsAccessibility: 'protected', kind: 'get'},
      {tsAccessibility: 'protected', kind: 'set'},
      {tsAccessibility: 'public', kind: 'get'},
      {tsAccessibility: 'public', kind: 'set'},
      {optional: false, override: false, static: false, computed: false},
    ]);
    expect(methods[12]).not.toHaveProperty('tsAccessibility');
  });

  test('declare export namespace specifiers', () => {
    const [declaration] = parse(
      'declare export * as Namespace from "module";',
    ).body;
    if (declaration.type !== 'DeclareExportDeclaration') {
      throw new Error('expected a declare export declaration');
    }

    expect(declaration.specifiers.some(isExportNamespaceSpecifier)).toBe(true);
  });
});

describe('non-null expression AST', () => {
  // Match TS ESLint's chain nesting, including repeated assertions and parentheses.
  test.each([
    ['value!!;', ['NonNullExpression', 'NonNullExpression', 'Identifier']],
    [
      'foo?.bar!;',
      [
        'ChainExpression',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!!;',
      [
        'ChainExpression',
        'NonNullExpression',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!();',
      [
        'ChainExpression',
        'CallExpression:false',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!.baz;',
      [
        'ChainExpression',
        'MemberExpression:false',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!!();',
      [
        'ChainExpression',
        'CallExpression:false',
        'NonNullExpression',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!?.baz;',
      [
        'ChainExpression',
        'MemberExpression:true',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      'foo?.bar!.baz!();',
      [
        'ChainExpression',
        'CallExpression:false',
        'NonNullExpression',
        'MemberExpression:false',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      '(foo?.bar)!();',
      [
        'CallExpression:false',
        'NonNullExpression',
        'ChainExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      '(foo?.bar)!.baz;',
      [
        'MemberExpression:false',
        'NonNullExpression',
        'ChainExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      '(foo?.bar!)!();',
      [
        'CallExpression:false',
        'NonNullExpression',
        'ChainExpression',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      '(foo?.bar!)?.baz;',
      [
        'ChainExpression',
        'MemberExpression:true',
        'ChainExpression',
        'NonNullExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
    [
      '(foo?.bar)!?.baz;',
      [
        'ChainExpression',
        'MemberExpression:true',
        'NonNullExpression',
        'ChainExpression',
        'MemberExpression:true',
        'Identifier',
      ],
    ],
  ])('preserves the expression path for %s', (code, expected) => {
    const [statement] = parse(code).body;
    if (statement.type !== 'ExpressionStatement') {
      throw new Error('expected an expression statement');
    }
    const path = [];
    let expression: ESNode = statement.expression;
    for (;;) {
      switch (expression.type) {
        case 'ChainExpression':
        case 'NonNullExpression':
          path.push(expression.type);
          expression = expression.expression;
          break;
        case 'CallExpression':
          path.push(`${expression.type}:${String(expression.optional)}`);
          expression = expression.callee;
          break;
        case 'MemberExpression':
          path.push(`${expression.type}:${String(expression.optional)}`);
          expression = expression.object;
          break;
        default:
          path.push(expression.type);
          expect(path).toEqual(expected); // OK
          return;
      }
    }
  });

  // A synthesized assertion ends at its bang, not at a following call or member.
  test.each([
    ['foo?.bar!();', ['foo?.bar!']],
    ['foo?.bar!.baz;', ['foo?.bar!']],
    ['foo?.bar!!();', ['foo?.bar!!', 'foo?.bar!']],
    ['foo?.bar!.baz!();', ['foo?.bar!.baz!', 'foo?.bar!']],
    ['foo?.bar /* before */ ! /* after */ .baz;', ['foo?.bar /* before */ !']],
    ['foo?.bar /* before */ !\n  (argument);', ['foo?.bar /* before */ !']],
    ['/* 😀 */ foo?.bar!();', ['foo?.bar!']],
  ])(
    'preserves assertion fields, traversal, and locations for %s',
    (code, expected) => {
      const assertions = [];
      SimpleTraverser.traverse(parse(code), {
        enter(node) {
          if (isNonNullExpression(node)) {
            expect(node).not.toHaveProperty('argument'); // OK
            expect(node).not.toHaveProperty('chain'); // OK
            expect(node.expression.range[1]).toBeLessThan(node.range[1]); // OK
            assertions.push(code.slice(...node.range));
            const prefix = code.slice(0, node.range[1]);
            const lines = prefix.split(/\r\n|[\n\r\u2028\u2029]/u);
            expect(node.loc.end).toEqual({
              line: lines.length,
              column: lines[lines.length - 1].length,
            }); // OK
          }
        },
        leave() {},
      });
      expect(assertions).toEqual(expected); // OK
    },
  );
});

test('Babel output treats `satisfies` like an `as` cast', async () => {
  expect(
    await printForSnapshotBabel('const value = expression satisfies SomeType;'),
  ).toMatchInlineSnapshot(`"const value = (expression: SomeType);"`);
});

test('Babel output removes abstract and override class syntax', async () => {
  expect(
    await printForSnapshotBabel(`
      abstract class Base {
        abstract property: string;
        abstract method(value: number): string;
      }
      class Derived extends Base {
        override property: string;
        override method(value: number): string {
          return value.toString();
        }
      }
    `),
  ).toMatchInlineSnapshot(`
   "class Base {}

   class Derived extends Base {
     property: string;

     method(value: number): string {
       return value.toString();
     }

   }"
  `);
});

test('Babel output lowers TypeScript CommonJS imports and exports', async () => {
  expect(
    await printForSnapshotBabel(`
      import Module = require("module");
      import Alias = Namespace.Member;
      export import Exported = Namespace.Member;
      import type TypeOnly = require("types");
      export = Module;
    `),
  ).toMatchInlineSnapshot(`
    "const Module = require(\"module\");

    var Alias = Namespace.Member;
    export var Exported = Namespace.Member;
    module.exports = Module;"
  `);
});

test('Babel output drops namespace exports and splits mixed exports', async () => {
  expect(
    await printForSnapshotBabel(`
      export as namespace Namespace;
      const value = 1;
      type T = number;
      export {value, type T};
    `),
  ).toMatchInlineSnapshot(`
    "const value = 1;
    type T = number;
    export { value };
    export type { T };"
  `);
});

test('Babel output lowers ambient function export assignments', async () => {
  expect(
    await printForSnapshotBabel(`
      declare module "module" {
        export = function fn(value: number): string;
      }
    `),
  ).toMatchInlineSnapshot(`
    "declare module \"module\" {
      declare function fn(value: number): string;
      declare module.exports: typeof fn
    }"
  `);
});

test('Babel output keeps import-equals and export assignments ambient', async () => {
  expect(
    await printForSnapshotBabel(`
      declare module "commonjs" {
        import Dependency = require("dependency");
        export = Dependency;
      }
      declare module "esm" {
        export import Alias = Namespace.Member;
      }
    `),
  ).toMatchInlineSnapshot(`
    "declare module \"commonjs\" {
      declare var Dependency: any;
      declare module.exports: typeof Dependency
    }
    declare module \"esm\" {
      declare export var Alias: any;
    }"
  `);
});
