// @flow

import {x} from './dependency';

(123: string); // error

// TODO: assert that this file being slow doesn't block file.js IDE commands
// $Flow$DebugSleep$DO_NOT_USE_IN_PRODUCTION_CODE_OR_YOU_WILL_BE_FIRED(3);
