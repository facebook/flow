// @flow

import { Bar } from "wat";

declare var wat: Bar;

;(wat.foo: number); // ok
;(wat.bar: string); // ok

const foo = [1, 2, 3];

(foo.bar: string); // ok
