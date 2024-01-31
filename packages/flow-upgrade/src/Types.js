/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {TransformVisitor} from 'hermes-transform';

export type CliOptions = $ReadOnly<{
  all: boolean,
  prettierOptions: $ReadOnly<{...}>,
  silent: boolean,
  yes: boolean,
}>;

export type CodemodModule = $ReadOnly<{
  kind: 'codemod',
  title: string,
  describe: string,
  transform: TransformVisitor,
}>;

export function codemod(config: {
  title: string,
  describe: string,
  transform: TransformVisitor,
}): CodemodModule {
  return {
    ...config,
    kind: 'codemod',
  };
}

export type Upgrade = $ReadOnly<{
  version: string,
  upgrades: $ReadOnlyArray<CodemodModule>,
}>;
