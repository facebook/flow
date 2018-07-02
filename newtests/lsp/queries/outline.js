// @flow

import type {Bar} from './references';

const WORD_REGEX = /\w+/gi;

type State =
  | 'Initial'
  | 'Starting';

type Preferences = {
  pref1?: Map<string, any>,
};

export type EPrefs = {
  pref2: number;
}

class MyClass1 {
  _projectRoot: string;
  command: string;

  constructor(
    param1: string) {
      this.command = param1;
    }

  dispose(): void {
  }
}

interface MyInterface2 {
  getFoo(): string;
}

function myFunction3() {}
