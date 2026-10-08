import type {ObjBound} from './bound_lookup_export';

declare const x: ObjBound;
x.b; // Error: `b` is missing in the bound
