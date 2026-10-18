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
export type NormalizeTag = 'paths' | 'json';

export type NormalizeOptions = Readonly<{
  normalize?: ReadonlyArray<NormalizeTag>,
}>;

export type CallSuggestion = Readonly<{
  method: AssertionMethod,
  args: ReadonlyArray<unknown>,
  ...
}>;

export type SnapshotSuggestion = Readonly<{
  file: string,
  contents: string,
  ...
}>;

export type Suggestion = CallSuggestion | SnapshotSuggestion;

export type ErrorAssertionResult =
  | Readonly<{
      type: 'pass',
      ...
    }>
  | Readonly<{
      type: 'fail',
      messages: ReadonlyArray<string>,
      assertLoc: ?AssertionLocation,
      suggestion: Suggestion,
      ...
    }>;

export type ErrorAssertion = (
  reason: ?string,
  state: StepEnvReadable,
) => ErrorAssertionResult;
