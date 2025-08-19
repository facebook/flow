// @flow

// dependent of b with suppressed error

import {b} from './b';

// $FlowFixMe[incompatible-type]
(123: string);
