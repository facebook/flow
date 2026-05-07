import {IndexedSeq} from './mixin';

declare const s: IndexedSeq<string>;
s.has("foo"); // Err: string ~> number (on Seq, _not_ IndexedIterable's inherited method)
