// @flow

import imported from './helpers/exports_annot';

type T = string

const x : T = "foo";

   x
// ^

   imported
// ^
