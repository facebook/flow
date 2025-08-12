// @flow strict-local

import {g} from './cycle1';

declare export function f(): $MyPreferredSuppression;

export default g(); // should be annotated with $MyPreferredSuppression
