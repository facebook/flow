// `writeonly` is a textual alias for `-` in property variance.

// 1. Object types — writeonly equivalent to -
type Wo = { writeonly p: number };
type Mn = { -p: number };
declare var wo: Wo;
declare var mn: Mn;
(wo: Mn); // ok
(mn: Wo); // ok

// 2. Class properties
class C {
  writeonly prop: number;
  constructor(p: number) {
    this.prop = p;
  }
}

// 3. Interfaces
interface I {
  writeonly prop: string;
}

// 4. Indexer
type Idx = { writeonly [string]: number };
type IdxMinus = { -[string]: number };
declare var ix: Idx;
(ix: IdxMinus); // ok

// 5. Tuples
type Tup = [writeonly a: string];

// 6. `writeonly` still usable as identifier name (regression)
const writeonly = 1;
type R = { writeonly: string };

// 7. writeonly property: reads error, writes succeed
type W = { writeonly p: number };
declare var w: W;
(w.p: number); // error: write-only
w.p = 0; // ok

// 8. Mapped types: +writeonly / writeonly add Negative variance
type Src = { foo: number, bar: string };
type AllWriteonly<T: {...}> = { +writeonly [K in keyof T]: T[K] };
declare const aw: AllWriteonly<Src>;
aw.foo as number; // error: writeonly property is not readable
aw.foo = 3; // ok

// 9. Mapped types: -writeonly removes Negative variance
type WriteOnlyO = { -foo: number, -bar: string };
type RemoveWriteonly<T: {...}> = { -writeonly [K in keyof T]: T[K] };
declare const rw: RemoveWriteonly<WriteOnlyO>;
rw.foo as number; // ok — writeonly was removed
rw.foo = 3; // ok
