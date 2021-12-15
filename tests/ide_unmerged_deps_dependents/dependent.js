// @flow

import {x} from './dependency';

(123: string); // error

// TODO: assert that this file being slow doesn't block file.js IDE commands
// declare var sleep: $Flow$DebugSleep;
// sleep(3);
