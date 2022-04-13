// @flow

import type {B} from './b';

export default class A {

  getValues<T>(): Array<T> {
    return [];
  }

  equals<T>(other: B<T>): any {
    return other.getValues();
//               ^
  }
}
