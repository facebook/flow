/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Tests, Steps} from './Tester';

export default class Suite {
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
