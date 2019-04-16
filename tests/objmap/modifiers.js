//@flow

type Id = <T>(T) => T;

type Partial<T: {}> = $ObjMap<T, Id, { optional: true }>;
type Required<T: {}> = $ObjMap<T, Id, { optional: false }>;

type Readonly<T: {}> = $ObjMap<T, Id, { polarity: '+' }>;
type Writeonly<T: {}> = $ObjMap<T, Id, { polarity: '-' }>;
type ReadWrite<T: {}> = $ObjMap<T, Id, { polarity: 'N' }>;

type T = {| a: number, b: string |};
type TP = {| a?: number, b ?: string |};
type TR = {| +a: number, +b: string |};
type TW = {| -a: number, -b: string |};
type TPR = {| +a ?: number, +b ?: string |};
type TPW = {| -a ?: number, -b ?: string |};

var v02: TP;
var v02: $ObjMap<T, Id, { optional: true }>;
var v02: Partial<T>;
var v02: $ObjMap<TP, Id>;

var v03: TR;
var v03: $ObjMap<T, Id, { polarity: '+' }>;
var v03: Readonly<T>;
var v03: $ObjMap<TR, Id>;

var v04: TW;
var v04: $ObjMap<T, Id, { polarity: '-' }>;
var v04: Writeonly<T>;
var v04: $ObjMap<TW, Id>;

var v05: TPR;
var v05: $ObjMap<T, Id, { optional: true, polarity: '+' }>;
var v05: Partial<TR>;
var v05: Readonly<TP>;
var v05: Partial<Readonly<T>>;
var v05: Readonly<Partial<T>>;
var v05: $ObjMap<TPR, Id>;

var v06: TPW;
var v06: $ObjMap<T, Id, { optional: true, polarity: '-' }>;
var v06: Partial<TW>;
var v06: Writeonly<TP>;
var v06: Partial<Writeonly<T>>;
var v06: Writeonly<Partial<T>>;
var v06: $ObjMap<TPW, Id>;

type Boxified<T> = $ObjMap<T, <P>(P) => { x: P }>;

type B = { a: { x: number }, b: { x: string } };
type BP = { a?: { x: number }, b?: { x: string } };
type BR = { +a: { x: number }, +b: { x: string } };
type BW = { -a: { x: number }, -b: { x: string } };
type BPR = { +a ?: { x: number }, +b ?: { x: string } };
type BPW = { -a ?: { x: number }, -b ?: { x: string } };

var b01: B;
var b01: $ObjMap<B, Id>;

var b02: BP;
var b02: $ObjMap<B, Id, { optional: true }>;
var b02: Partial<B>;
var b02: $ObjMap<BP, Id>;

var b03: BR;
var b03: $ObjMap<B, Id, { polarity: '+' }>;
var b03: Readonly<B>;
var b03: $ObjMap<BR, Id>;

var b04: BW;
var b04: $ObjMap<B, Id, { polarity: '-' }>;
var b04: Writeonly<B>;
var b04: $ObjMap<BW, Id>;

var b05: BPR;
var b05: $ObjMap<B, Id, { optional: true, polarity: '+' }>;
var b05: Partial<BR>;
var b05: Readonly<BP>;
var b05: Partial<Readonly<B>>;
var b05: Readonly<Partial<B>>;
var b05: $ObjMap<BPR, Id>;

var b06: BPW;
var b06: $ObjMap<B, Id, { optional: true, polarity: '-' }>;
var b06: Partial<BW>;
var b06: Writeonly<BP>;
var b06: Partial<Writeonly<B>>;
var b06: Writeonly<Partial<B>>;
var b06: $ObjMap<BPW, Id>;

type Foo = { prop: number, [x: string]: number };

function f1(x: Partial<Foo>) {
  x.prop; // ok
  (x["other"] || 0).toFixed();
}

function f2(x: Readonly<Foo>) {
  x.prop; // ok
  x["other"].toFixed();
}

function f3(x: Boxified<Foo>) {
  x.prop; // ok
  x["other"].x.toFixed();
}

function f4(x: $ObjMap<Foo, Id>) {
  x.prop; // ok
  x["other"].toFixed();
}

var foo1: T;
var foo1: $ObjMap<T, Id, {}>;
var foo1: $ObjMap<T, Id, null>;
var foo1: $ObjMap<T, Id, boolean>;
var foo1: $ObjMap<T, Id, string>;
var foo1: $ObjMap<T, Id, string>;

type Config = { polarity: '+' }
var foo2: { +a: string }; // not ok
var foo2: $ObjMap<{ a: string }, Id, Config>; // TODO: doesn't support aliases / generics

type Polarity<T: {}, K: '-' | '+' | 'N' > = $ObjMap<T, Id, { polarity: K }>;
var foo3: { +a: string }; // not ok
var foo3: Polarity<{ a: string }, '+'>; // TODO: doesn't support aliases / generics

var foo4: {+a: 'a', +b: 'b'}
var foo4: $ObjMapi<{ a: string, b: string }, <K>(K) => K, { polarity: '+' }>

var foo5: { a: string, b: string };
var foo5: ReadWrite<{+a: string, +b: string}>;
var foo5: ReadWrite<{-a: string, +b: string}>;
var foo5: ReadWrite<{+a: string, -b: string}>;
var foo5: ReadWrite<{-a: string, -b: string}>;

var foo6: { a: string, b: string };
var foo6: Required<{ a?: string, b?: string }>;

var dict0: { +[key: string]: number };
var dict0: Readonly<{ [key: string]: number }>;

var dict1: { -[key: string]: number };
var dict1: Writeonly<{ [key: string]: number }>;

var dict2: { [key: string]: number };
var dict2: Partial<{ [key: string]: number }>;

var dict3: { [key: string]: number };
var dict3: Required<{ [key: string]: number }>;