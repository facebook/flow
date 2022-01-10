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

const {difference, prettyPrint} = require('../../flowResult');

import type {
  AssertionLocation,
  ErrorAssertion,
  ErrorAssertionResult,
} from './assertionTypes';
import type {FlowResult} from '../../flowResult';

function noNewErrors(assertLoc: ?AssertionLocation): ErrorAssertion {
  return (reason: ?string, env): ErrorAssertionResult => {
    const brandNew = difference(env.getNewErrors(), env.getOldErrors());
    if (!brandNew.passed) {
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
        chalk.green('Actual new errors (+)') +
          chalk.grey(" didn't match expected no new errors"),
      ];
      const errorMessages = prettyPrint(brandNew)
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
        method: 'newErrors',
        args: [prettyPrint(brandNew)],
      };

      return {type: 'fail', messages, assertLoc, suggestion};
    }
    return {type: 'pass'};
  };
}

module.exports = {
  default: noNewErrors,
};
