// @flow

import {F} from './declared';

F.M as F; // OK
F.M as boolean; // ERROR

import {H} from 'declared-module';

H.X as H; // OK
H.X as boolean; // ERROR
