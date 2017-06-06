/* @flow */

import type {X} from './test-module';

type Y = {name: 'Y'};

class Clazz<T1: Object, T2: Object> {
  method<t1: T1, t2: T2>(func: () => t1 | t2) { // fix by flipping the order of `t1` and `t2` here
    func();
  }
}

const r: Clazz<X, Y> = new Clazz(); // or by flipping the order of `X` and `Y` here
r.method((): X => ({name: 'X'}));
r.method((): Y => ({name: 'Y'}));
