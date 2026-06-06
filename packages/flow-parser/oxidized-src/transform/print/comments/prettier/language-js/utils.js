/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

/**
 * @param {Comment} comment
 * @returns {boolean}
 */
function isBlockComment(comment) {
  return (
    comment.type === 'Block' ||
    comment.type === 'CommentBlock' ||
    // `meriyah`
    comment.type === 'MultiLine'
  );
}

/**
 * @param {Comment} comment
 * @returns {boolean}
 */
function isLineComment(comment) {
  return (
    comment.type === 'Line' ||
    comment.type === 'CommentLine' ||
    // `meriyah` has `SingleLine`, `HashbangComment`, `HTMLOpen`, and `HTMLClose`
    comment.type === 'SingleLine' ||
    comment.type === 'HashbangComment' ||
    comment.type === 'HTMLOpen' ||
    comment.type === 'HTMLClose'
  );
}

/**
 * @param {Node} node
 * @returns {boolean}
 */
function isCallExpression(node) {
  return (
    node &&
    (node.type === 'CallExpression' || node.type === 'OptionalCallExpression')
  );
}

/**
 * @param {Node} node
 * @returns {boolean}
 */
function isMemberExpression(node) {
  return (
    node &&
    (node.type === 'MemberExpression' ||
      node.type === 'OptionalMemberExpression')
  );
}

const functionParametersCache = new WeakMap();
function getFunctionParameters(node) {
  if (functionParametersCache.has(node)) {
    return functionParametersCache.get(node);
  }
  const parameters = [];
  if (node.this) {
    parameters.push(node.this);
  }
  // `params` vs `parameters` - see https://github.com/babel/babel/issues/9231
  if (Array.isArray(node.parameters)) {
    parameters.push(...node.parameters);
  } else if (Array.isArray(node.params)) {
    parameters.push(...node.params);
  }
  if (node.rest) {
    parameters.push(node.rest);
  }
  functionParametersCache.set(node, parameters);
  return parameters;
}

const callArgumentsCache = new WeakMap();
function getCallArguments(node) {
  if (callArgumentsCache.has(node)) {
    return callArgumentsCache.get(node);
  }

  let args = node.arguments;
  if (node.type === 'ImportExpression') {
    args = [node.source];

    if (node.attributes) {
      args.push(node.attributes);
    }
  }

  callArgumentsCache.set(node, args);
  return args;
}

function isPrettierIgnoreComment(comment) {
  return comment.value.trim() === 'prettier-ignore' && !comment.unignore;
}

function isCallLikeExpression(node) {
  return (
    isCallExpression(node) ||
    node.type === 'NewExpression' ||
    node.type === 'ImportExpression'
  );
}

function isObjectProperty(node) {
  return (
    node &&
    (node.type === 'ObjectProperty' ||
      (node.type === 'Property' && !node.method && node.kind === 'init'))
  );
}

module.exports = {
  getFunctionParameters,
  getCallArguments,
  isBlockComment,
  isCallLikeExpression,
  isLineComment,
  isPrettierIgnoreComment,
  isCallExpression,
  isMemberExpression,
  isObjectProperty,
};
