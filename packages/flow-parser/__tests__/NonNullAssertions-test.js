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
import * as FlowParser from '../oxidized-src/FlowParser';
import {SimpleTraverser} from '../oxidized-src/traverse/SimpleTraverser';

const FLOW_MODES: Array<'all' | 'detect'> = ['all', 'detect'];

// A line terminator before ! separates assertions from prefix negation.
describe.each([false, true])('non-null assertion ASI, tokens=%s', tokens => {
  test.each([
    '\n',
    '\r',
    '\r\n',
    '\u2028',
    '\u2029',
    '// comment\n',
    '/*\n*/',
    '/*\r*/',
    '/*\u2028*/',
    '/*\u2029*/',
  ])('line terminator %j', separator => {
    for (const enableTypes of [false, true]) {
      for (const value of ['value', 'value?.prop', 'value?.call()']) {
        for (const negated of ['other', 'check()', 'other.prop']) {
          const code = `${value}${separator}!${negated};`;
          const ast = enableTypes
            ? parse(code, {tokens})
            : FlowParser.parse(code, {enableTypes: false, tokens});
          if (ast.type !== 'Program') {
            throw new Error('expected an ESTree program');
          }
          expect(ast.body).toHaveLength(2);
          expect(ast.body[0]).toMatchObject({type: 'ExpressionStatement'});
          expect(ast.body[1]).toMatchObject({
            type: 'ExpressionStatement',
            expression: {type: 'UnaryExpression', operator: '!', prefix: true},
          });
        }
      }
    }
  });

  // Same-line assertions are always parsed when types are enabled.
  test.each([
    'value!',
    'value!!',
    'value?.prop!',
    'value?.prop!!',
    'value?.prop!.next',
    'value?.call!()',
  ])('default-on assertion %s', value => {
    expect(parse(`${value};`, {tokens}).body).toHaveLength(1);
    expect(parse(`${value};`, {tokens, flow: 'detect'}).body).toHaveLength(1);
    expect(() =>
      FlowParser.parse(`${value};`, {tokens, enableTypes: false}),
    ).toThrow();
  });

  // A multiline literal may be asserted on its ending line, but not the next line.
  test.each(['`a\nb`', '`a${1}b\nc`', '"a' + '\\' + '\n' + 'b"'])(
    'assertion after multiline token %s',
    literal => {
      for (const flow of FLOW_MODES) {
        expect(parse(`${literal}!;`, {tokens, flow}).body[0]).toMatchObject({
          type: 'ExpressionStatement',
          expression: {type: 'NonNullExpression'},
        });
        expect(parse(`${literal}!.length;`, {tokens, flow}).body).toHaveLength(
          1,
        );
        expect(parse(`${literal}\n!other;`, {tokens, flow}).body).toHaveLength(
          2,
        );
      }
    },
  );

  // Failed type-argument speculation must not swallow a preceding assertion.
  test.each([
    ['value?.prop! < other;', '<', 1],
    ['value?.prop!! < other;', '<', 2],
    ['value?.prop! /* comparison */ < other;', '<', 1],
    ['value?.prop! <= other;', '<=', 1],
    ['value?.prop! << other;', '<<', 1],
    ['value?.prop! > other;', '>', 1],
  ])('comparison after assertion %s', (code, operator, assertionCount) => {
    for (const flow of FLOW_MODES) {
      const ast = parse(code, {tokens, flow});
      const statement = ast.body[0];
      if (statement.type !== 'ExpressionStatement') {
        throw new Error('expected an expression statement');
      }
      expect(statement.expression).toMatchObject({
        type: 'BinaryExpression',
        operator,
        left: {
          type: 'ChainExpression',
          expression: {
            type: 'NonNullExpression',
            range: [0, 11 + assertionCount],
          },
        },
      });
      let assertions = 0;
      SimpleTraverser.traverse(ast, {
        enter(node) {
          if (node.type === 'NonNullExpression') assertions += 1;
        },
        leave() {},
      });
      expect(assertions).toBe(assertionCount);
    }
  });

  // Successful type arguments preserve exactly one assertion around the callee.
  test('asserted generic optional-chain call', () => {
    expect(parse('value?.method!<number>(1);', {tokens}).body[0]).toMatchObject(
      {
        type: 'ExpressionStatement',
        expression: {
          type: 'ChainExpression',
          expression: {
            type: 'CallExpression',
            callee: {
              type: 'NonNullExpression',
              expression: {type: 'MemberExpression', optional: true},
            },
          },
        },
      },
    );
  });

  // Assertion spans include the bang, not surrounding Unicode trivia.
  test.each(['\n', '\r', '\r\n', '\u2028', '\u2029'])(
    'assertion locations after %j',
    separator => {
      for (const trivia of [
        '',
        '\t',
        ' /* ! */ ',
        '\u00a0',
        '\ufeff',
        '\u2003',
      ]) {
        const prefix = `x;${separator}`;
        const receiver = 'foo?.bar';
        const ast = parse(`${prefix}${receiver}${trivia}!.baz;`, {tokens});
        expect(ast.body[1]).toMatchObject({
          type: 'ExpressionStatement',
          expression: {
            type: 'ChainExpression',
            expression: {
              type: 'MemberExpression',
              object: {
                type: 'NonNullExpression',
                range: [
                  prefix.length,
                  prefix.length + receiver.length + trivia.length + 1,
                ],
                loc: {
                  start: {line: 2, column: 0},
                  end: {line: 2, column: receiver.length + trivia.length + 1},
                },
              },
            },
          },
        });
      }
    },
  );

  // Trivia after the assertion may contain a line terminator.
  test.each(['value!\n.prop', 'value?.prop!\n.next', 'value?.call!\n()'])(
    'continuation after assertion %s',
    value => {
      expect(parse(`${value};`, {tokens}).body).toHaveLength(1);
    },
  );
});
