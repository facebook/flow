/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

// Minimal type stubs for relay-runtime, used in relay_core evals.

export type OperationType = Readonly<{
  variables: {...},
  response: {...},
  ...
}>;

export interface FragmentType {}

export type MutationParameters = {
  variables: {...},
  response: {...},
  rawResponse?: {...},
  ...
};

export type GraphQLTaggedNode = Readonly<{
  kind: string,
  ...
}>;

export type Disposable = {
  dispose: () => void,
  ...
};

export type PayloadError = {
  message: string,
  locations?: ReadonlyArray<{line: number, column: number, ...}>,
  ...
};
