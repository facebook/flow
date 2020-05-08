// @flow strict-local

import {f} from './other_cycle2';

declare export function g(): $MyPreferedSuppression;

export default f(); // should be annotated with $MyPreferedSuppression
