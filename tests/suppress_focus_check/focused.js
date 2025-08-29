// @flow

import {foo} from './dependency';

foo("hello"); // No error

// $FlowFixMe[random] - Unused suppression
