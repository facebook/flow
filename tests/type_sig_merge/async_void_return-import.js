// @flow

import {f} from './async_void_return-export';

(f(): empty); // error: Promise ~> empty
