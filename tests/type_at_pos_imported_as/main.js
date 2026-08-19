// @flow

import { C as CLocal } from './exports';

import type { T as TLocal } from './exports';

import type { I as ILocal } from './exports';

import { E as ELocal } from './exports';

import { Comp as CompLocal } from './exports';

type __ = [
    CLocal,
//  ^
    TLocal,
//  ^
    ILocal,
//  ^
    ELocal,
//  ^
    CompLocal,
//  ^
];

// An imported name reached through another type: the hover prints the name it was
// imported as, so that is the name the `defined at` footer has to cite. Citing the
// name at the definition site instead would point at something the hover never
// mentions.
type Nested = {c: CLocal, t: TLocal};
//   ^
