// @flow

import {g} from './other_cycle1';

declare export function f(): any;

export default g(); // any okay
