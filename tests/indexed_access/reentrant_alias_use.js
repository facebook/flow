import type {Args} from './reentrant_alias_def';

declare const a: Args;
a[0] as number;
a[0] as string; // ERROR
