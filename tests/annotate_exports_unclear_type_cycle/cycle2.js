// @flow strict-local

import {g} from './cycle1';

declare export function f(): $MyPreferedSuppression;

export default g(); // should be annotated with $MyPreferedSuppression
