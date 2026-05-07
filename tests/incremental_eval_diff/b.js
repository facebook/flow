// @flow

import type {A} from './a';

declare const a: A;
(a as {
    m: number,
    n: number,
});
