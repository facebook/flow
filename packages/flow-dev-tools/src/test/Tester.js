/**
 * @flow
 * @format
 */

import {format} from 'util';

import Suite from './Suite';
import Test from './Test';

import type {TestStep, TestStepFirstStage} from './TestStep';

export type StepList = Array<TestStep>;
export type Steps = (tester: TestStepFirstStage) => StepList;
export type Tests = (tester: TestStepFirstStage) => Array<Test>;

export function suite(tests: Tests): Suite {
  return new Suite(tests);
}

export function test(name: string, steps: StepList): Test {
  return new Test(name, steps);
}
