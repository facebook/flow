// @flow

import type A from './a';

export default class P<T> {
  getValues(): Array<T> {
    return [];
  }
}

export type B<T> = P<T> | A;
