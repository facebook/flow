// @flow

// import a type from cycle_b
import type {B} from './cycle_b';

// export a type for cycle_b to use
export type A = string;

// export a value for cycle_b to use
export const a: B = 'a';

// import a value from dependency, so that editing
// dependency dirties this file
import {dependency} from './dependency';

// $FlowFixMe[incompatible-cast]
(1: string); // suppressed error
