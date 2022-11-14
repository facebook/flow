// @flow

import {F} from './declared';

(F.M: F); // OK
(F.M: boolean); // ERROR


import {H} from 'declared-module';

(H.X: H); // OK
(H.X: boolean); // ERROR
