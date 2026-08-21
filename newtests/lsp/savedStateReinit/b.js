// @flow

import {foo} from './a';

export function useFoo(): number {
  return foo.bar();
}
