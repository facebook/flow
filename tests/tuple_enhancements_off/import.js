import type {T, S, R} from './test';

declare const x: mixed;
(x: T); // OK: `T` is `any`
(x: S); // OK: `S` is `any`
(x: R); // OK: `R` is `any`
