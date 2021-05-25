//@flow

import type { T } from './upstream';

class C {
  x : string
}

declare var x : C;

(x : T);

import { Foo } from './upstream';

let f = (new Foo()).f;
