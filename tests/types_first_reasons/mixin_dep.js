// @flow

import {IndexedSeq} from './mixin';

declare var s: IndexedSeq<string>;
s.has("foo"); // Err: string ~> number (on Seq, _not_ IndexedIterable's inherited method)
