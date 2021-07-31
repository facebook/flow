/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {defaultFlowConfigName} from '../constants';

import type {StepList} from './Tester';

export default class Test {
  flowConfigFilename: string = defaultFlowConfigName;
  name: ?string;
  steps: StepList;
  tags: Array<string>;
  lazyMode: 'ide' | 'fs' | null = null;
  shouldWaitForRecheck: boolean = true;

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
}
