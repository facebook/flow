// An indexed access through a `unique symbol` key resolves to that member's
// type, both over an object type and over an interface.
import {Keys} from './keys';
import type {I} from './keys';

declare const x: I[typeof Keys.a];
x as number; // OK
x as string; // ERROR: the member keyed by `Keys.a` is `number`, not `string`

type Obj = {[Keys.b]: string};
declare const y: Obj[typeof Keys.b];
y as string; // OK
y as number; // ERROR: the member keyed by `Keys.b` is `string`, not `number`
