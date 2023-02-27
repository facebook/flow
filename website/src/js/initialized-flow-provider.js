/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {spawn} = require('child_process');

async function checkContents(input /*: string */) {
  const flowProcess = spawn(process.env.FLOW_BIN_PATH || 'flow', [
    'check-contents',
    '--json',
    '--flowconfig-name',
    '.flowconfig.snippets',
  ]);
  flowProcess.stdin.end(input, 'utf8');
  let json = '';
  for await (const data of flowProcess.stdout) {
    json += data.toString();
  }
  return JSON.parse(json);
}

module.exports = async function getFlowErrors(
  code /*: string */,
) /*: Promise<string> */ {
  if (process.env.NO_INLINE_FLOW_ERRORS) {
    return '[]';
  }
  return JSON.stringify(
    (await checkContents(code)).errors
      .flatMap(({message}) => message)
      .map(({loc, descr}) => ({
        startLine: loc.start.line,
        startColumn: loc.start.column,
        endLine: loc.end.line,
        endColumn: loc.end.column,
        description: descr,
      })),
  );
};
