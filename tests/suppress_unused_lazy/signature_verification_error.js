// @flow

// dependent of b with signature verification error

import {b} from './b';

// unsuppressed signature-verification-error
export function foo(x: number) { return x; }
