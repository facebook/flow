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

const chalk = require('chalk');

import type {
  AssertionLocation,
  ErrorAssertion,
  ErrorAssertionResult,
} from './assertionTypes';

function lspNoNewMessagesAfterSleep(
  timeoutMs: number,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env): ErrorAssertionResult => {
    const actual = env.getLSPMessagesSinceStartOfStep();
    if (actual.length > 0) {
      const locMessage =
        assertLoc == null
          ? []
          : [
              format(
                chalk.white('%s line %d col %d'),
                assertLoc.filename,
                assertLoc.line,
                assertLoc.column,
              ),
            ];
      const keyMessage = [
        chalk.green('Actual LSP messages (+)') +
          chalk.grey(" didn't match expected no new LSP messages"),
      ];
      const errorMessages = JSON.stringify(actual, null, 2)
        .split('\n')
        .map(line => chalk.green('+ ' + line));
      const reasonMessage =
        reason == null
          ? []
          : [format(chalk.grey('Reason: ') + chalk.red('%s'), reason)];
      const messages = [].concat(
        locMessage,
        reasonMessage,
        keyMessage,
        errorMessages,
      );
      const suggestion = {
        method: 'waitAndVerifyAllLSPMessagesContentSinceStartOfStep',
        args: [timeoutMs * 10, actual],
      };

      return {type: 'fail', messages, assertLoc, suggestion};
    }
    return {type: 'pass'};
  };
}

module.exports = {
  default: lspNoNewMessagesAfterSleep,
};
