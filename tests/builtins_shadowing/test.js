import type {Array} from './array-exports';
const v1: Array = "";
v1 as empty; // error: string ~> empty

type $Call = number;
const v2: $Call = 3;
v2 as empty; // error: number ~> empty
