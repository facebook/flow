// @flow

import type {A} from './a';

declare var a: A;
(a as {
    m: number,
    n: number,
});
