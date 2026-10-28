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

import {parse} from '../../__test_utils__/parse';
import {print} from 'flow-transform';
import {transformProgram as stripFlowTypes} from '../../oxidized-src/estree/StripFlowTypes';

describe('StripFlowTypes', () => {
  test('strips non-null expressions', () => {
    // Removing the assertion preserves the expression and its original location.
    const ast = parse('value!;');
    const stripped = stripFlowTypes(ast, {});

    expect(stripped.body[0]).toMatchObject({
      type: 'ExpressionStatement',
      expression: {type: 'Identifier', name: 'value', range: [0, 5]},
    }); // OK
  });

  test.each([
    ['foo?.bar!;', 'foo?.bar;'],
    ['foo?.bar!!;', 'foo?.bar;'],
    ['value!!;', 'value;'],
    ['foo?.bar!();', 'foo?.bar();'],
    ['foo?.bar!.baz;', 'foo?.bar.baz;'],
    ['foo?.bar![key];', 'foo?.bar[key];'],
    ['foo?.bar!.baz.qux;', 'foo?.bar.baz.qux;'],
    ['foo?.bar!.baz();', 'foo?.bar.baz();'],
    ['foo?.bar!.baz!();', 'foo?.bar.baz();'],
    ['foo?.bar!!();', 'foo?.bar();'],
    ['foo?.bar!!.baz;', 'foo?.bar.baz;'],
    ['foo?.bar!?.baz;', 'foo?.bar?.baz;'],
    ['(foo?.bar)!();', '(foo?.bar)();'],
    ['(foo?.bar)!.baz;', '(foo?.bar).baz;'],
    ['(foo?.bar!)!();', '(foo?.bar)();'],
    ['(foo?.bar!)?.baz;', 'foo?.bar?.baz;'],
    ['(foo?.bar)!?.baz;', 'foo?.bar?.baz;'],
    ['foo?.[key!]!(value!);', 'foo?.[key](value);'],
    ['(foo?.bar!.baz).qux;', '(foo?.bar.baz).qux;'],
  ])(
    'preserves optional-chain boundaries when stripping %s',
    async (code, result) => {
      // Assertions do not end a chain; parentheses still do.
      const stripped = stripFlowTypes(parse(code), {});

      expect((await print(stripped, code)).trim()).toBe(result); // OK
    },
  );

  // Nested chains survive erasure, even if the printer can omit redundant parentheses.
  test.each(['(foo?.bar!)?.baz;', '(foo?.bar)!?.baz;'])(
    'retains the parenthesized receiver AST for %s',
    code => {
      const stripped = stripFlowTypes(parse(code), {});
      expect(stripped.body[0]).toMatchObject({
        type: 'ExpressionStatement',
        expression: {
          type: 'ChainExpression',
          expression: {
            type: 'MemberExpression',
            object: {type: 'ChainExpression'},
          },
        },
      }); // OK
    },
  );

  test('strips tagged-template type arguments', () => {
    const ast = parse('tag<T>`value`;');
    const stripped = stripFlowTypes(ast, {});
    const statement = stripped.body[0];
    if (
      statement.type !== 'ExpressionStatement' ||
      statement.expression.type !== 'TaggedTemplateExpression'
    ) {
      throw new Error('expected a tagged template expression');
    }

    expect(statement.expression.typeArguments).toBeNull();
  });
});
