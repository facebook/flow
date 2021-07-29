/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+flow
 * @format
 */

'use strict';

module.exports = {
  meta: {
    messages: {
      message:
        'Use a function with a switch statement rather than an object literal to map Flow Enums to other values.',
    },
  },
  create(context) {
    return {
      ObjectExpression(node) {
        const {properties} = node;
        if (
          !properties.length ||
          !properties.every(
            prop =>
              prop.type === 'Property' &&
              prop.computed &&
              isEnumStringCast(prop.key),
          )
        ) {
          return;
        }
        context.report({
          node,
          messageId: 'message',
        });
      },
    };
  },
};

/**
 * Member expression with identifier property.
 */
function isEnumAccess(node) {
  return (
    node != null &&
    node.type === 'MemberExpression' &&
    node.property.type === 'Identifier' &&
    !node.computed
  );
}

/**
 * Either
 * `String(Foo.A)`
 * or
 * `(Foo.A: string)`
 */
function isEnumStringCast(node) {
  if (node.type === 'CallExpression') {
    const {callee, arguments: args} = node;
    return (
      callee.type === 'Identifier' &&
      callee.name === 'String' &&
      isEnumAccess(args[0])
    );
  } else if (node.type === 'TypeCastExpression') {
    const {typeAnnotation, expression} = node;
    return (
      typeAnnotation.typeAnnotation.type == 'StringTypeAnnotation' &&
      isEnumAccess(expression)
    );
  }
  return false;
}
