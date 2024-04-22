type T = [number, ...]; // ERROR

{
  declare const x: mixed;
  x as T; // OK - `T` is `any`
}

import type {Exported} from './export';
{
  declare const x: mixed;
  x as Exported; // OK - `Exported` is `any`
}
