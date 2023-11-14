/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {StepList} from './Tester';

export interface Test {
  flowConfigFilename: string;
  name: ?string;
  steps: StepList;
  tags: Array<string>;
  lazyMode: 'ide' | 'fs' | null;
  shouldWaitForRecheck: boolean;
  noRestart: boolean;
}
