/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {format} = require('util');

const {Suite} = require('./Suite');
const {Test} = require('./Test');

import type {TestStep, TestStepFirstStage} from './TestStep';

export type StepList = Array<TestStep>;
export type Steps = (tester: TestStepFirstStage) => StepList;
export type Tests = (tester: TestStepFirstStage) => Array<Test>;

function suite(tests: Tests): Suite {
  return new Suite(tests);
}

function test(name: string, steps: StepList): Test {
  return new Test(name, steps);
}

module.exports = {
  suite,
  test,
};
