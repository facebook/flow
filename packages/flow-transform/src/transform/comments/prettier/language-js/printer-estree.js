/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const handleComments = require('./comments.js');
const {isBlockComment, isLineComment} = require('./utils.js');

function canAttachComment(node) {
  return (
    node.type &&
    !isBlockComment(node) &&
    !isLineComment(node) &&
    node.type !== 'EmptyStatement' &&
    node.type !== 'TemplateElement' &&
    node.type !== 'Import' &&
    // `babel-ts` don't have similar node for `class Foo { bar() /* bat */; }`
    node.type !== 'TSEmptyBodyFunctionExpression'
  );
}

module.exports = {
  canAttachComment,
  handleComments: {
    avoidAstMutation: true,
    ownLine: handleComments.handleOwnLineComment,
    endOfLine: handleComments.handleEndOfLineComment,
    remaining: handleComments.handleRemainingComment,
  },
  getCommentChildNodes: handleComments.getCommentChildNodes,
};
