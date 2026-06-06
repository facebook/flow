/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {StatementTypes} from './test-utils';

import * as t from '../../../src/generated/node-types';
import {
  createInsertStatementMutation,
  performInsertStatementMutation,
} from '../../../src/transform/mutations/InsertStatement';
import {MutationContext} from '../../../src/transform/MutationContext';
import {
  CODE_SAMPLES,
  DEFAULT_SKIP_STATEMENTS,
  LOOP_ONLY_STATEMENTS,
  MODULE_DECLARATIONS,
  testStatementMutation,
} from './test-utils';

function test({
  wrapCode,
  getAssertionObject,
  skipTypes,
}: {
  wrapCode: (code: string) => string,
  getAssertionObject: (nodes: [mixed, mixed]) => mixed,
  skipTypes?: $ReadOnlyArray<StatementTypes>,
}) {
  function loopSamples(side: 'before' | 'after') {
    describe(side, () => {
      testStatementMutation({
        wrapCode,
        mutateAndAssert: (ast, target) => {
          const nodeToInsert = t.ExpressionStatement({
            expression: t.Identifier({
              name: 'inserted',
            }),
          });
          const mutation = createInsertStatementMutation(side, target, [
            nodeToInsert,
          ]);
          if (mutation == null) {
            throw new Error('this is impossible!?');
          }
          performInsertStatementMutation(new MutationContext(''), mutation);
          expect(ast).toMatchObject(
            getAssertionObject(
              side === 'before'
                ? [nodeToInsert, target]
                : [target, nodeToInsert],
            ),
          );
        },
        skipTypes,
      });
    });
  }

  loopSamples('before');

  loopSamples('after');
}

describe('InsertStatement', () => {
  describe('Parent: Program', () => {
    test({
      wrapCode: c => c,
      getAssertionObject: body => ({
        type: 'Program',
        body,
      }),
      // we are allowed to have ModuleDeclarations here
      skipTypes: LOOP_ONLY_STATEMENTS,
    });
  });

  describe('Parent: IfStatement', () => {
    describe('BlockStatement body', () => {
      test({
        wrapCode: c => `if (cond) { ${c} }`,
        getAssertionObject: body => ({
          type: 'Program',
          body: [
            {
              type: 'IfStatement',
              consequent: {
                type: 'BlockStatement',
                body,
              },
            },
          ],
        }),
      });
    });

    describe('Statement body', () => {
      test({
        wrapCode: c => `if (cond) ${c}`,
        getAssertionObject: body => ({
          type: 'Program',
          body: [
            {
              type: 'IfStatement',
              consequent: {
                type: 'BlockStatement',
                body,
              },
            },
          ],
        }),
        skipTypes: [
          // all of these are non-sensical without a body
          'BlockStatement',
          'TypeAlias',
          'OpaqueType',
          'InterfaceDeclaration',
          'FunctionDeclaration',
          'VariableDeclaration',
          'ClassDeclaration',
          'DeclareTypeAlias',
          'DeclareOpaqueType',
          'DeclareInterface',
          'DeclareModule',
          'DeclareNamespace',
          ...DEFAULT_SKIP_STATEMENTS,
        ],
      });
    });
  });

  describe('Parent: ForStatement', () => {
    describe('BlockStatement body', () => {
      test({
        wrapCode: c => `for (;;) { ${c} }`,
        getAssertionObject: body => ({
          type: 'Program',
          body: [
            {
              type: 'ForStatement',
              body: {
                type: 'BlockStatement',
                body,
              },
            },
          ],
        }),
        skipTypes: MODULE_DECLARATIONS,
      });
    });

    describe('Statement body', () => {
      test({
        wrapCode: c => `for (;;) ${c}`,
        getAssertionObject: body => ({
          type: 'Program',
          body: [
            {
              type: 'ForStatement',
              body: {
                type: 'BlockStatement',
                body,
              },
            },
          ],
        }),
        skipTypes: [
          // all of these are non-sensical without a body
          'BlockStatement',
          'TypeAlias',
          'OpaqueType',
          'InterfaceDeclaration',
          'FunctionDeclaration',
          'VariableDeclaration',
          'ClassDeclaration',
          'DeclareTypeAlias',
          'DeclareOpaqueType',
          'DeclareInterface',
          'DeclareModule',
          'DeclareNamespace',
          ...MODULE_DECLARATIONS,
        ],
      });
    });
  });

  describe('Parent: SwitchCase', () => {
    test({
      wrapCode: c => `switch (thing) { case c: ${c} }`,
      getAssertionObject: body => ({
        type: 'Program',
        body: [
          {
            type: 'SwitchStatement',
            cases: [
              {
                type: 'SwitchCase',
                consequent: body,
              },
            ],
          },
        ],
      }),
      // BreakStatements are allowed inside a SwitchCase
      skipTypes: ['ContinueStatement', ...MODULE_DECLARATIONS],
    });
  });

  describe('Parent: DeclareModule', () => {
    test({
      wrapCode: c => `declare module foo { ${c} }`,
      getAssertionObject: body => ({
        type: 'Program',
        body: [
          {
            type: 'DeclareModule',
            body: {
              type: 'BlockStatement',
              body,
            },
          },
        ],
      }),
      // only "declare" statements are allowed
      skipTypes: Object.keys(CODE_SAMPLES).filter(
        t => !t.startsWith('Declare'),
      ),
    });
  });
});
