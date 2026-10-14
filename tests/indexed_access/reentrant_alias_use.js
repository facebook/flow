import type {Args} from './reentrant_alias_def';

declare const a: Args;
a[0]; // ok
