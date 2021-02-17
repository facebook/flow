// @flow

import def, {x, y, z} from './helpers/exports_annot';

   def
// ^

   x
// ^

   y
// ^

   z
// ^


type T = string
const t : T = "foo";

   t
// ^
