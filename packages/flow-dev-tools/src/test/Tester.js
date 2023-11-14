/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Test} from './Test';
import type {TestStep, TestStepFirstStage} from './TestStep';

export type StepList = Array<TestStep>;
export type Steps = (tester: TestStepFirstStage) => StepList;
export type Tests = (tester: TestStepFirstStage) => Array<Test>;
