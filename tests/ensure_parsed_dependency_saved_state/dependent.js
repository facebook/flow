// @flow

import {foo} from './dependency';

// this file should not get checked because of lazy mode, so the next line
// should NOT error
(123: string); // no error!
