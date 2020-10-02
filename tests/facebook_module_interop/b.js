// @flow

import * as A from './a';

(A.default: string); // Error: cannot cast number to string
(A.foo: string); // Error: cannot cast number to string
