//@flow

import type { T } from './upstream';

class C {
  x : string
}

declare const x : C;

x as T;
