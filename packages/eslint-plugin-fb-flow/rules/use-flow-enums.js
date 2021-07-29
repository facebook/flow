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

function isIdentifier(node, expectedName) {
  return (
    node.type === 'Identifier' &&
    (expectedName === undefined || node.name === expectedName)
  );
}

function isLegacyEnum(node) {
  const {callee, arguments: args} = node;
  // Legacy enums: `keyMirror({...})` and `Object.freeze({...})`
  const literalTypes = new Set(['TemplateLiteral', 'Literal']);
  const uniqueValues = new Set();
  const uniqueKeys = new Set();
  const uniqueTypes = new Set();
  if (
    // `keyMirror(...)` or `Object.freeze(...)`
    (isIdentifier(callee, 'keyMirror') ||
      (callee.type === 'MemberExpression' &&
        isIdentifier(callee.object, 'Object') &&
        isIdentifier(callee.property, 'freeze'))) &&
    // A single argument that's an object literal.
    args.length === 1 &&
    args[0].type === 'ObjectExpression' &&
    args[0].properties.length > 0 &&
    args[0].properties.every(prop => {
      const isLiteral =
        !prop.computed &&
        !prop.shorthand &&
        !prop.method &&
        prop.key != null &&
        prop.value != null &&
        isIdentifier(prop.key) &&
        // Valid enum patterns names must start with an uppercase [A-Z]
        /^[A-Z]/.test(prop.key.name) &&
        literalTypes.has(prop.value.type) &&
        // String literals that don't follow this regex pattern are probably
        // just part of a bag of constants rather than an enum-like pattern.
        (prop.value.type !== 'Literal' ||
          /^[-a-zA-Z_0-9]*$/.test(prop.value.value));

      if (prop.value != null && prop.value.value != null) {
        uniqueValues.add(prop.value.value);
      } else if (prop.key) {
        // if the value is null, assume a keyMirror and increment the Set()
        uniqueValues.add(prop.key.name);
      }

      if (prop.key != null) {
        uniqueKeys.add(prop.key.name);
      }

      if (prop.value != null) {
        uniqueTypes.add(typeof prop.value.value);
      }

      return isLiteral;
    }) &&
    // Check all keys are unique
    uniqueKeys.size === args[0].properties.length &&
    // Check all values are unique
    uniqueValues.size === args[0].properties.length &&
    // The `type` of each property value is the same.
    uniqueTypes.size === 1
  ) {
    return true;
  }
  return false;
}

module.exports = {
  meta: {
    messages: {
      useFlowEnumsObjectFreeze:
        'Use Flow Enums when creating enum objects. Ex. `enum Foo { {{key}} = {{value}}, ... }`',
      useFlowEnumsKeyMirror:
        'Use Flow Enums when creating enum objects. Ex. `enum Foo { {{key}}, ... }`',
    },
  },
  create(context) {
    return {
      CallExpression(node) {
        if (isLegacyEnum(node)) {
          const key = node.arguments[0].properties[0].key.name;
          const value = node.arguments[0].properties[0].value.value;
          if (value) {
            context.report({
              node,
              messageId: 'useFlowEnumsObjectFreeze',
              data: {
                key,
                value,
              },
            });
          } else {
            context.report({
              node,
              messageId: 'useFlowEnumsKeyMirror',
              data: {
                key,
              },
            });
          }
        }
      },
    };
  },
};
