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

function isFileWithAmbiguousObjectTypes(context) {
  const comments = context.getSourceCode().getAllComments();
  return comments.some(comment =>
    /\bflowlint\s+.*\bambiguous-object-type:error/.test(comment.value),
  );
}

const rule = {
  meta: {
    messages: {
      message:
        'Object types are now exact by default. You should prefer the { prop: type } syntax over {| prop: type |}.',
    },
    fixable: 'code',
  },
  create(context) {
    return {
      'ObjectTypeAnnotation[exact=true]'(node) {
        // We do not want to trigger this rule within files that have
        // ambiguous object types (e.g. files that are synced/shared to projects
        // which do not have exact by default turned on)
        if (isFileWithAmbiguousObjectTypes(context)) {
          return;
        }

        context.report({
          node,
          messageId: 'message',
          fix(fixer) {
            const [rangeStart, rangeEnd] = node.range;

            const openingBraceRange = [rangeStart, rangeStart + 2];
            const closingBraceRange = [rangeEnd - 2, rangeEnd];

            return [
              fixer.replaceTextRange(openingBraceRange, '{'),
              fixer.replaceTextRange(closingBraceRange, '}'),
            ];
          },
        });
      },
    };
  },
};

module.exports = rule;
