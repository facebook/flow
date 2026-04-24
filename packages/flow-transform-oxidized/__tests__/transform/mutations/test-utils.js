/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {
  ESNode,
  ModuleDeclaration,
  Program,
  Statement,
} from 'flow-estree-oxidized';

import {traverse} from '../../../src/traverse/traverse';
import {parse} from '../../../src/transform/parse';

export type StatementTypes = Statement['type'] | ModuleDeclaration['type'];
export const CODE_SAMPLES: $ReadOnly<{[StatementTypes]: string}> = {
  ExpressionStatement: 'foo;',
  BlockStatement: '{}',
  EmptyStatement: ';',
  DebuggerStatement: 'debugger;',
  WithStatement: 'with (foo) {}',
  ReturnStatement: 'return foo;',
  LabeledStatement: 'label: foo;',
  BreakStatement: 'break;',
  ContinueStatement: 'continue;',
  IfStatement: 'if (foo) {}',
  SwitchStatement: 'switch (foo) {}',
  ThrowStatement: 'throw foo;',
  TryStatement: 'try {} catch {}',
  WhileStatement: 'while (foo) {}',
  DoWhileStatement: 'do {} while (foo)',
  ForStatement: 'for (;;) {}',
  ForInStatement: 'for (foo in bar) {}',
  ForOfStatement: 'for (foo of bar) {}',
  TypeAlias: 'type Foo = 1;',
  OpaqueType: 'opaque type Foo = 1;',
  InterfaceDeclaration: 'interface Foo {}',
  FunctionDeclaration: 'function foo() {}',
  VariableDeclaration: 'let foo;',
  ClassDeclaration: 'class Foo {}',
  DeclareTypeAlias: 'declare type Foo = 1;',
  DeclareOpaqueType: 'declare opaque type Foo: 1;',
  DeclareInterface: 'declare interface Foo {}',
  DeclareModule: 'declare module foo {}',
  ImportDeclaration: "import foo from 'foo';",
  ExportNamedDeclaration: 'export { foo };',
  ExportDefaultDeclaration: 'export default foo;',
  ExportAllDeclaration: "export * from 'foo';",
  DeclareExportDeclaration: 'declare export default foo;',
  DeclareExportAllDeclaration: "declare export * from 'foo';",
  DeclareModuleExports: 'declare module.exports: Foo;',
};
export const MODULE_DECLARATIONS: $ReadOnlyArray<ModuleDeclaration['type']> = [
  'ImportDeclaration',
  'ExportNamedDeclaration',
  'ExportDefaultDeclaration',
  'ExportAllDeclaration',
  'DeclareExportDeclaration',
  'DeclareExportAllDeclaration',
  'DeclareModuleExports',
];
export const LOOP_ONLY_STATEMENTS: $ReadOnlyArray<
  'BreakStatement' | 'ContinueStatement',
> = ['BreakStatement', 'ContinueStatement'];
export const DEFAULT_SKIP_STATEMENTS: $ReadOnlyArray<StatementTypes> = [
  ...MODULE_DECLARATIONS,
  ...LOOP_ONLY_STATEMENTS,
];

export async function parseAndGetAstAndNode<T: ESNode = ESNode>(
  type: ESNode['type'],
  code: string,
): Promise<{
  ast: Program,
  target: T,
}> {
  const {ast, scopeManager} = await parse(code);

  let target: T | null = null;
  traverse(code, ast, scopeManager, () => ({
    // $FlowExpectedError[invalid-computed-prop] - this is guaranteed safe
    // $FlowFixMe[incompatible-type]
    [type](node: T | null) {
      target = node;
    },
  }));
  if (target == null) {
    throw new Error(`Couldn't find a ${type} node in the parsed AST.`);
  }

  return {ast, target};
}

export function testStatementMutation({
  wrapCode,
  mutateAndAssert,
  skipTypes = DEFAULT_SKIP_STATEMENTS,
}: {
  wrapCode: (code: string) => string,
  mutateAndAssert: (
    ast: Program,
    target: Statement | ModuleDeclaration,
  ) => void,
  skipTypes?: $ReadOnlyArray<StatementTypes>,
}) {
  for (const type of Object.keys(CODE_SAMPLES)) {
    if (skipTypes.includes(type)) {
      continue;
    }

    it(type, async () => {
      const {ast, target} = await parseAndGetAstAndNode<
        Statement | ModuleDeclaration,
      >(type, wrapCode(CODE_SAMPLES[type]));
      mutateAndAssert(ast, target);
    });
  }
}
