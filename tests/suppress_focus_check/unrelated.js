// @flow

import {foo} from './dependency';

// $FlowFixMe[incompatible-call] - Suppresses error in unrelated.js
foo(123);
