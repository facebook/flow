/* @flow */

import {defaultFlowConfigName} from '../constants';

import type {StepList} from './Tester';

export default class Test {
  flowConfigFilename: string = defaultFlowConfigName;
  name: ?string;
  steps: StepList;
  tags: Array<string>;
  lazyMode: 'ide' | 'fs' | null = null;

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
}
