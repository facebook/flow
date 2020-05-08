// @flow

import {f} from './cycle2';

declare export function g(): any;

export default f(); // any okay
