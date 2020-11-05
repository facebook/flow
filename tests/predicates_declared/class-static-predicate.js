// @flow

import {Seq, isSeq} from './class-static-predicate-lib'

declare var foo: string | Seq;

if (Seq.isSeq(foo)) {
  (foo: Seq);
} else {
  (foo: string);
}

if (isSeq(foo)) {
  (foo: Seq);
} else {
  (foo: string);
}
