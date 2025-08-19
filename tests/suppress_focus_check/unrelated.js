// @flow

import {foo} from './dependency';

// $FlowFixMe[incompatible-type] - Suppresses error in unrelated.js
foo(123);
