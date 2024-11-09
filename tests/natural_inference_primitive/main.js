// @flow

import {
  abc,
  one,
  tru,
  bigOne,
  abcRef,
  oneRef,
  truRef,
  bigOneRef,
  obj,
  spread,
  objRefs,
  spreadObjRefs,
  asConst,
  type Abc,
  type One,
  type Tru,
  type BigOne,
  type ObjRefs,
  type SpreadObjRefs,
  type ObjWithTypeofAbc,
} from './es6';

import default_ from './default';

import cjs1 from './cjs-1';
import cjs2 from './cjs-2';

declare var _: any;

abc as "abc"; // okay
abc as "def" // error "abc" ~> "def"
_ as "abc" as typeof abc; // okay
_ as "def" as typeof abc; // error "def" ~> "abc"

one as 1; // okay
one as 2 // error 1 ~> 2
_ as 1 as typeof one; // okay
_ as 2 as typeof one; // error 2 ~> 1

tru as true; // okay
tru as false // error true ~> false
_ as true as typeof tru; // okay
_ as false as typeof tru; // error false ~> true

bigOne as 1n; // okay
bigOne as 2n // error 1n ~> 2n
_ as 1n as typeof bigOne; // okay
_ as 2n as typeof bigOne; // error 2n ~> 1n

abcRef as "abc"; // okay
abcRef as "def" // error "abc" ~> "def"
_ as "abc" as typeof abcRef; // okay
_ as "def" as typeof abcRef; // error "def" ~> "abc"

oneRef as 1; // okay
oneRef as 2 // error 1 ~> 2
_ as 1 as typeof oneRef; // okay
_ as 2 as typeof oneRef; // error 2 ~> 1

truRef as true; // okay
truRef as false // error true ~> false
_ as true as typeof truRef; // okay
_ as false as typeof truRef; // error false ~> true

bigOneRef as 1n; // okay
bigOneRef as 2n // error 1n ~> 2n
_ as 1n as typeof bigOneRef; // okay
_ as 2n as typeof bigOneRef; // error 2n ~> 1n

obj.abc as "abc"; // error string ~> "abc"
obj.one as 1; // error number ~> 1
obj.tru as true; // error boolean ~> true
obj.bigOne as 1n; // error bigint ~> 1n

spread.abc as "abc"; // error string ~> "abc"
spread.one as 1; // error number ~> 1
spread.tru as true; // error boolean ~> true
spread.bigOne as 1n; // error bigint ~> 1n

objRefs.abc as "abc"; // error string ~> "abc"
objRefs.abcRef as "abc"; // error string ~> "abc"
spreadObjRefs.abc as "abc"; // error string ~> "abc"
spreadObjRefs.abcRef as "abc"; // error string ~> "abc"

asConst.abc as "abc"; // okay
asConst.abcRef as "abc"; // okay
asConst.abc as "def"; // error  "abc" ~> "def"
asConst.abcRef as "def"; // error "abc" ~> "def"
_ as "def" as typeof asConst.abc; // error "def" ~> "abc"
_ as "def" as typeof asConst.abcRef; // error "def" ~> "abc"
_ as string as typeof asConst.abc; // error string ~> "abc"
_ as string as typeof asConst.abcRef; // error string ~> "abc"

_ as "abc" as Abc as "abc"; // okay
_ as 1 as One as 1; // okay
_ as true as Tru as true; // okay
_ as 1n as BigOne as 1n; // okay

function test_typeof_obj(x: ObjRefs) {
  x.abc as "abc"; // error string ~> "abc"
  x.abcRef as "abc"; // error string ~> "abc"
}

function test_Spread(x: SpreadObjRefs) {
  x.abc as "abc"; // error string ~> "abc"
  x.abcRef as "abc"; // error string ~> "abc"
}

function test_obj_with_typeof_abc(x: ObjWithTypeofAbc) {
  x.f as "abc"; // okay
}

function test_default(x: typeof default_) {
  x as "abc"; // error string ~> "abc"
}

// CommonJS primitive literals are inferred as their general type
function test_cjs1() {
  cjs1 as "abc"; // error string ~> "abc"
}

function test_cjs2() {
  cjs2.foo as "abc"; // error string ~> "abc"
  cjs2.bar as 1; // error number ~> 1
  cjs2.baz as true; // error boolean ~> true
}
