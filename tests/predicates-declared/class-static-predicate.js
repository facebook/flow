// @flow

import {Seq} from './class-static-predicate-lib'

declare var m: string | Seq<number, string>;

// TODO: should refine
if (Seq.isSeq(m)) {
  m.size // Cannot get `m.size` because property `size` is missing in  `String` 
} else {
  ;(m: string);
}