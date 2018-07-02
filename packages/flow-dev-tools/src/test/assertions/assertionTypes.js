/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import type {StepEnvReadable} from '../stepEnv';

export type AssertionLocation = {
  filename: string,
  line: number,
  column: number,
};

type AssertionMethod =
  | 'noNewErrors'
  | 'newErrors'
  | 'stdout'
  | 'stderr'
  | 'sortedStdout'
  | 'exitCodes'
  | 'ideStartAndConnect'
  | 'verifyServerStatus'
  | 'verifyIDEStatus'
  | 'verifyAllIDEMessagesInStep'
  | 'waitAndVerifyAllIDEMessagesContentSinceStartOfStep'
  | 'waitAndVerifyNoIDEMessagesSinceStartOfStep'
  | 'ideStderr';

export type Suggestion = {
  method: AssertionMethod,
  args: Array<mixed>,
};

export type ErrorAssertionResult =
  | {
      type: 'pass',
    }
  | {
      type: 'fail',
      messages: Array<string>,
      assertLoc: ?AssertionLocation,
      suggestion: Suggestion,
    };

export type ErrorAssertion = (
  reason: ?string,
  state: StepEnvReadable,
) => ErrorAssertionResult;
