/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  ...
};

export type AssertionMethod =
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
  | 'lspStderr'
  | 'verifyFileContents'
  | 'verifyNoFile';

// Closed set of output-normalization tags an assertion may request via `{normalize: [...]}`.
export type NormalizeTag = 'paths';

export type NormalizeOptions = $ReadOnly<{
  normalize?: $ReadOnlyArray<NormalizeTag>,
}>;

export type CallSuggestion = $ReadOnly<{
  method: AssertionMethod,
  args: $ReadOnlyArray<mixed>,
  ...
}>;

export type SnapshotSuggestion = $ReadOnly<{
  file: string,
  contents: string,
  ...
}>;

export type Suggestion = CallSuggestion | SnapshotSuggestion;

export type ErrorAssertionResult =
  | $ReadOnly<{
      type: 'pass',
      ...
    }>
  | $ReadOnly<{
      type: 'fail',
      messages: $ReadOnlyArray<string>,
      assertLoc: ?AssertionLocation,
      suggestion: Suggestion,
      ...
    }>;

export type ErrorAssertion = (
  reason: ?string,
  state: StepEnvReadable,
) => ErrorAssertionResult;
