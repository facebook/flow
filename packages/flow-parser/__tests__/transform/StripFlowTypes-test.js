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
    const ast = parse('value!;', {assertOperator: true});
    const stripped = stripFlowTypes(ast, {});

    expect(stripped.body[0]).toMatchObject({
      type: 'ExpressionStatement',
      expression: {type: 'Identifier', name: 'value'},
    });
  });

  test.each([
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
    ['(foo?.bar!.baz).qux;', '(foo?.bar.baz).qux;'],
  ])(
    'preserves optional-chain boundaries when stripping %s',
    async (code, result) => {
      const stripped = stripFlowTypes(parse(code, {assertOperator: true}), {});

      expect((await print(stripped, code)).trim()).toBe(result);
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
