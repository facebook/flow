/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {format} from 'util';

import colors from 'colors/safe';

import type {
  AssertionLocation,
  ErrorAssertion,
  ErrorAssertionResult,
} from './assertionTypes';

export default function(
  timeoutMs: number,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env): ErrorAssertionResult => {
    const actual = env.getIDEMessagesSinceStartOfStep();
    if (actual.length > 0) {
      const locMessage =
        assertLoc == null
          ? []
          : [
              format(
                colors.white('%s line %d col %d'),
                assertLoc.filename,
                assertLoc.line,
                assertLoc.column,
              ),
            ];
      const keyMessage = [
        colors.green('Actual IDE messages (+)') +
          colors.grey(" didn't match expected no new IDE messages"),
      ];
      const errorMessages = JSON.stringify(actual, null, 2)
        .split('\n')
        .map(line => colors.green('+ ' + line));
      const reasonMessage =
        reason == null
          ? []
          : [format(colors.grey('Reason: ') + colors.red('%s'), reason)];
      const messages = [].concat(
        locMessage,
        reasonMessage,
        keyMessage,
        errorMessages,
      );
      const suggestion = {
        method: 'waitAndVerifyAllIDEMessagesContentSinceStartOfStep',
        args: [timeoutMs * 10, actual],
      };

      return {type: 'fail', messages, assertLoc, suggestion};
    }
    return {type: 'pass'};
  };
}
