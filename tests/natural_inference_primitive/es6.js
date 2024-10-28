// @flow

export const abc = "abc";
export const one = 1;
export const tru = true;
export const bigOne = 1n;

export const abcRef = abc;
export const oneRef = one;
export const truRef = tru;
export const bigOneRef = bigOne;

export const obj = { abc: "abc", one: 1, tru: true, bigOne: 1n };
export const spread = { ...obj };

export const objRefs = { abc, abcRef };
export const spreadObjRefs = { ...objRefs };

export type Abc = typeof abc;
export type One = typeof one;
export type Tru = typeof tru;
export type BigOne = typeof bigOne;
export type ObjRefs = typeof objRefs;
export type SpreadObjRefs = typeof spreadObjRefs;

export type ObjWithTypeofAbc = { f: typeof abc };

declare var _: any;

abc as "abc"; // okay
abc as "def" // error "abc" ~> "def"
_ as "abc" as typeof abc; // okay
_ as "def" as typeof abc; // TODO error "def" ~> "abc"

one as 1; // okay
one as 2 // error 1 ~> 2
_ as 1 as typeof one; // okay
_ as 2 as typeof one; // TODO error 2 ~> 1

tru as true; // okay
tru as false // error true ~> false
_ as true as typeof tru; // okay
_ as false as typeof tru; // TODO error false ~> true

bigOne as 1n; // okay
bigOne as 2n // error 1n ~> 2n
_ as 1n as typeof bigOne; // okay
_ as 2n as typeof bigOne; // TODO error 2n ~> 1n

abcRef as "abc"; // okay
abcRef as "def" // error "abc" ~> "def"
_ as "abc" as typeof abcRef; // okay
_ as "def" as typeof abcRef; // TODO error "def" ~> "abc"

oneRef as 1; // okay
oneRef as 2 // error 1 ~> 2
_ as 1 as typeof oneRef; // okay
_ as 2 as typeof oneRef; // TODO error 2 ~> 1

truRef as true; // okay
truRef as false // error true ~> false
_ as true as typeof truRef; // okay
_ as false as typeof truRef; // TODO error false ~> true

bigOneRef as 1n; // okay
bigOneRef as 2n // error 1n ~> 2n
_ as 1n as typeof bigOneRef; // okay
_ as 2n as typeof bigOneRef; // TODO error 2n ~> 1n

obj.abc as "abc"; // TODO error string ~> "abc"
obj.one as 1; // TODO error number ~> 1
obj.tru as true; // TODO error boolean ~> true
obj.bigOne as 1n; // TODO error bigint ~> 1n

spread.abc as "abc"; // TODO error string ~> "abc"
spread.one as 1; // TODO error number ~> 1
spread.tru as true; // TODO error boolean ~> true
spread.bigOne as 1n; // TODO error bigint ~> 1n

objRefs.abc as "abc"; // TODO error string ~> "abc"
objRefs.abcRef as "abc"; // TODO error string ~> "abc"
spreadObjRefs.abc as "abc"; // TODO error string ~> "abc"
spreadObjRefs.abcRef as "abc"; // TODO error string ~> "abc"

_ as "abc" as Abc as "abc"; // okay
_ as 1 as One as 1; // okay
_ as true as Tru as true; // okay
_ as 1n as BigOne as 1n; // okay

function test_typeof_obj(x: ObjRefs) {
  x.abc as "abc"; // TODO error string ~> "abc"
  x.abcRef as "abc"; // TODO error string ~> "abc"
}

function test_Spread(x: SpreadObjRefs) {
  x.abc as "abc"; // TODO error string ~> "abc"
  x.abcRef as "abc"; // TODO error string ~> "abc"
}

function test_obj_with_typeof_abc(x: ObjWithTypeofAbc) {
  x.f as "abc"; // okay
}
