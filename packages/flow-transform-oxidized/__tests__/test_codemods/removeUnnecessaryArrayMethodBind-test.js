/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {t, transform} from './test-utils';

const ARRAY_ITERATOR_METHODS = new Set([
  'every',
  'filter',
  'forEach',
  'map',
  'some',
  // reduce / reduceRight don't accept a context argument
]);

function codemod(code: string) {
  return transform(code, context => {
    return {
      CallExpression(node) {
        if (
          node.callee.type !== 'MemberExpression' ||
          node.callee.computed === true ||
          node.callee.property.type !== 'Identifier' ||
          !ARRAY_ITERATOR_METHODS.has(node.callee.property.name) ||
          node.arguments.length !== 2
        ) {
          return;
        }
        const callback = node.arguments[0];
        const thisContext = node.arguments[1];

        switch (callback.type) {
          case 'ArrowFunctionExpression':
            // arr.forEach(() => {}, this);
            //                       ^^^^ remove
            context.modifyNodeInPlace(node, {
              arguments: [node.arguments[0]],
            });
            break;

          case 'FunctionExpression':
            if (callback.generator) {
              // this isn't possible to directly convert to an arrow function
              // the second arg binding is really the only solution
              // but really nobody should ever be using an array iterator with a
              // generator function - that's just weird.
              return;
            }
            if (thisContext.type !== 'ThisExpression') {
              // nobody should be doing anything dumb like this to bind to a weird
              // this context, but it is valid code that can't be done with an
              // arrow function
              return;
            }
            // arr.forEach(function foo() {}, this);
            //                                ^^^^ remove
            //             ^^^^^^^^^^^^^^^^^ convert to arrow
            context.modifyNodeInPlace(node, {
              arguments: [
                t.ArrowFunctionExpression({
                  async: callback.async,
                  body: callback.body,
                  params: callback.params,
                  predicate: callback.predicate,
                  returnType: callback.returnType,
                  typeParameters: callback.typeParameters,
                }),
              ],
            });
        }
      },
    };
  });
}

describe('remove unnecessary array method bind', () => {
  it('should work', async () => {
    const result = await codemod(`\
/**
 */

export default class Component {
  method() {
    return LINKS.map(function (d, idx) {
      return 1;
    }, this);
  }
}
`);

    expect(result).toBe(`\
/**
 */

export default class Component {
  method() {
    return LINKS.map((d, idx) => {
      return 1;
    });
  }
}
`);
  });
});
