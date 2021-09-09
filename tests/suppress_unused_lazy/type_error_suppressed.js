// @flow

// dependent of b with suppressed error

import {b} from './b';

// $FlowFixMe[incompatible-cast]
(123: string);
