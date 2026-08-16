// A mapped type with an `as` remapping clause keeps distinct `unique symbol`
// destinations distinct rather than collapsing them into a single indexer.
import {Keys} from './keys';
import type {I} from './keys';

type M = {[K in keyof I as K]: I[K]};
declare const m: M;
m[Keys.a] as number; // OK
m[Keys.b] as string; // OK
m[Keys.a] as string; // ERROR: number is not string (a did not merge with b)
