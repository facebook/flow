/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {format} = require('util');
const {exec} = require('../utils/async');

/**
 * Q: Now that we have hermes-parser already in the flow-dev-tools, why are we still shelling out to
 * flow-bin?
 * A: Unicode!
 *
 * flow-bin and hermes-parser handle the offset differently in the presence of longer-than-1-byte
 * characters. flow-bin will treat everything as if only ASCII exists, while hermes-parser will do
 * the sensible thing of recording the offset in the same way ordinary human would do (e.g. ’ is
 * treated as a single character).
 *
 * We have some existing code that builds on top of the behavior of flow-bin by normalizing
 * everything into byte buffer, so that all the operations are done on bytes instead of characters.
 * You might think that we can just use hermes-parser directly and remove all these garbage, but
 * we do get the error location and offset from flow-bin for error suppression and removal, so we
 * still need to use flow-bin as the source of truth...
 *
 * Here ends our journey of moving away from `flow ast` :(
 */
async function getAst(
  code: string,
  flowBinPath: string,
): Promise<Object> /* AST */ {
  const stdout = await exec(format('%s ast', flowBinPath), {
    maxBuffer: 16 * 1024 * 1024,
    stdin: code,
  });
  return JSON.parse(stdout);
}

module.exports = {
  default: getAst,
};
