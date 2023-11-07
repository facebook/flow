//@flow

import {y} from './cycle';

export function test(x: number): typeof y {
  x as typeof y;
  return x;
}
