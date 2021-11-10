/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
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
  | 'lspStartAndConnect'
  | 'verifyServerStatus'
  | 'verifyLSPStatus'
  | 'verifyAllLSPMessagesInStep'
  | 'verifyLSPMessageSnapshot'
  | 'verifyMockInvocationsSinceStartOfStepContaining'
  | 'waitAndVerifyAllLSPMessagesContentSinceStartOfStep'
  | 'waitAndVerifyNoLSPMessagesSinceStartOfStep'
  | 'lspStderr';

export type CallSuggestion = {|
  method: AssertionMethod,
  args: Array<mixed>,
|};

export type SnapshotSuggestion = {|
  file: string,
  contents: string,
|};

export type Suggestion = CallSuggestion | SnapshotSuggestion;

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
