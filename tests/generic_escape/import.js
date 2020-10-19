//@flow

import {id} from './export';

var x = 42;

function f<T>(t: T, escape: boolean): T | number {
  if (escape) {
    x = id(t);
    x = id<T>(t);
  }
  return x;
}
