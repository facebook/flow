// @flow

// With the flag disabled (default), Flow keeps its legacy behaviour of
// forwarding the default through `export *`, so this resolves (no error).
import DefaultThroughStar from './barrel';
DefaultThroughStar as number; // ok under legacy behaviour

import {named} from './barrel';
named as number; // ok
