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
  meta: {fixable: 'code'},
  create(context) {
    function report(node) {
      context.report({
        node,
        message:
          'All your member values are strings which mirror their member name. This is the default behavior if you omit a member value, so you should omit it.',
        fix(fixer) {
          return node.members.map(member =>
            fixer.replaceText(member, member.id.name),
          );
        },
      });
    }

    return {
      EnumStringBody(node) {
        if (
          node.members.length &&
          node.members.every(
            member =>
              member.type === 'EnumStringMember' &&
              member.id.name === member.init.value,
          )
        ) {
          report(node);
        }
      },
    };
  },
};
