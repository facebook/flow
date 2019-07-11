// @flow

import { Bar } from "wat";

declare var wat: Bar;

wat.foo; // doesn't work, but probably should, maybe not
wat.bar;

const foo = [1, 2, 3];

(foo.bar: string); // ok
