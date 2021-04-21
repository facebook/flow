import type {MaybeZ} from './optional';

(1: MaybeZ?.['c']); // OK
(undefined: MaybeZ?.['c']); // OK
(true: MaybeZ?.['c']); // Error - wrong type
