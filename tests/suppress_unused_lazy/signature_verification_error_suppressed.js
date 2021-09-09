// @flow

// dependent of b with a suppressed signature verification error

import {b} from './b';

// $FlowFixMe[signature-verification-failure]
export function foo(x: number) { return x; }
