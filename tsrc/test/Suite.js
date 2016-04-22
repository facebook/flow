/* @flow */

import type {Tests, Steps} from './Tester';

export default class Suite {
  beforeEach: Steps;
  tags: Array<string>;
  tests: Tests;

  constructor(tests: Tests) {
    this.tests = tests;
    this.tags = [];
    this.beforeEach = () => [];
  }

  beforeEach(steps: Steps): this {
    this.beforeEach = steps;
    return this;
  }

  addTags(tags: Array<string>): this {
    this.tags = (this.tags || []).concat(tags);
    return this;
  }
}
