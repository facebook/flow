/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {
  TestStep,
  TestStepFirstStage,
} from 'flow-dev-tools/src/test/TestStep';
import type {Test as ITest} from 'flow-dev-tools/src/test/Test';
import type {Suite as ISuite} from 'flow-dev-tools/src/test/Suite';

type StepList = Array<TestStep>;
type Steps = (tester: TestStepFirstStage) => StepList;
type Tests = (tester: TestStepFirstStage) => Array<ITest>;

const {format} = require('util');

class Suite implements ISuite {
  __SUITE__: boolean = true;
  getBeforeEach: Steps;
  tags: Array<string>;
  tests: Tests;

  constructor(tests: Tests) {
    this.tests = tests;
    this.tags = [];
    this.getBeforeEach = () => [];
  }

  beforeEach(steps: Steps): this {
    this.getBeforeEach = steps;
    return this;
  }

  addTags(tags: Array<string>): this {
    this.tags = (this.tags || []).concat(tags);
    return this;
  }
}

class Test implements ITest {
  flowConfigFilename: string = '_flowconfig';
  name: ?string;
  steps: StepList;
  tags: Array<string>;
  lazyMode: 'ide' | 'fs' | null = null;
  shouldWaitForRecheck: boolean = true;
  noRestart: boolean = false;

  constructor(name: string, steps: StepList) {
    this.name = name;
    this.steps = steps;
    this.tags = [];
  }

  addTags(tags: Array<string>): this {
    this.tags = (this.tags || []).concat(tags);
    return this;
  }

  flowConfig(filename: string): this {
    this.flowConfigFilename = filename;
    return this;
  }

  lazy(mode: 'ide' | 'fs'): this {
    this.lazyMode = mode;
    return this;
  }

  waitForRecheck(wait_for_recheck: boolean): this {
    this.shouldWaitForRecheck = wait_for_recheck;
    return this;
  }

  noAutoRestart(): this {
    this.noRestart = true;
    return this;
  }
}

function suite(tests: Tests): Suite {
  return new Suite(tests);
}

function test(name: string, steps: StepList): Test {
  return new Test(name, steps);
}

export type SuiteType = Suite;

module.exports = {
  suite,
  test,
};
