// @flow

// import a type from cycle_a
import type {A} from './cycle_a';

// export a type for cycle_a to use
export type B = string;

// export a value for cycle_a to use
export const b: A = 'b';

// $FlowFixMe[incompatible-type]
1 as string; // suppressed error

1 as string; // error
