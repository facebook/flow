/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
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
