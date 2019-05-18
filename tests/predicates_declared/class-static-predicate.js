// @flow

import {Seq, isSeq} from './class-static-predicate-lib'

declare var foo: string | Seq<number, string>;

// TODO: should refine
if (Seq.isSeq(foo)) {
  foo.size // Cannot get `m.size` because property `size` is missing in  `String` 
} else {
  ;(foo: string);
}

// TODO: should refine
if (isSeq(foo)) {
  foo.size // Cannot get `m.size` because property `size` is missing in  `String` 
} else {
  ;(foo: string);
}
