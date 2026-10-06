// Missing neutral optional properties checked through an opaque type's upper bound

import type {Nested, Direct} from './opaque_export';

declare const nested: Nested;
const n: {readonly f: {a: number, b?: number}} = nested; // ERROR

declare const direct: Direct;
const d: {a: number, b?: number} = direct; // ERROR
