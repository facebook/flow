import type {MaybeZ} from './optional';

1 as MaybeZ?.['c']; // OK
undefined as MaybeZ?.['c']; // OK
true as MaybeZ?.['c']; // Error - wrong type
