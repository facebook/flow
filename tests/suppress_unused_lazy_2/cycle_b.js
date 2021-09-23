// @flow

// import a type from cycle_a
import type {A} from './cycle_a';

// export a type for cycle_a to use
export type B = string;

// export a value for cycle_a to use
export const b: A = 'b';

// $FlowFixMe[incompatible-cast]
(1: string); // suppressed error

(1: string); // error
