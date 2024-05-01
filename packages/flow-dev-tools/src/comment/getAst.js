/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const HermesParser = require('hermes-parser');
const {format} = require('util');
const {exec} = require('../utils/async');

function getAst(code: string): Object /* AST */ {
  return HermesParser.parse(code, {babel: false});
}

module.exports = {
  default: getAst,
};
