// @flow

import {x} from './dependency';

(x: number); // error

declare var sleep: $Flow$DebugSleep;
sleep(3);
