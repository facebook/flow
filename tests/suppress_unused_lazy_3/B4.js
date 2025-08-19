// @flow

import {b2} from './B2';
import type {B2} from './B2';

// $FlowFixMe[incompatible-type]
(b2: string);

("": B2); // this will cause an error in B2
