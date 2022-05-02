/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {spawnSync} = require('child_process');

const checkContents = input =>
  JSON.parse(
    spawnSync(
      'flow',
      ['check-contents', '--json', '--flowconfig-name', '.flowconfig.snippets'],
      {
        input,
        encoding: 'utf8',
      },
    ).stdout.toString(),
  );

module.exports = function getFlowErrors(code /*: string */) /*: string[] */ {
  return checkContents(code)
    .errors.flatMap(({message}) => message)
    .map(
      ({loc, descr}) =>
        `${loc.start.line}:${loc.start.column}-${loc.end.line}:${loc.end.column}: ${descr}`,
    );
};
