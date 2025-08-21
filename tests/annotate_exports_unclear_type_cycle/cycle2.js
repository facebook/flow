// @flow strict-local

import {g} from './cycle1';

declare export function f(): $FlowFixMe;

export default g(); // should be annotated with $FlowFixMe
