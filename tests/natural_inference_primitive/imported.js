// @flow

import {
  abc,
  def,
  one,
  tru,
  bigOne,
  abcRef,
  oneRef,
  truRef,
  bigOneRef,
  as_const,
  prop1,
  prop2,
  prop3,
  obj,
  spread,
  objRefs,
  spreadObjRefs,
  asConst,
} from "./local";

import type {
  Abc,
  One,
  Tru,
  BigOne,
  ObjRefs,
  SpreadObjRefs,
  ObjWithTypeofAbc,
} from './local';

// Local checks

declare var _: any;

function test_string() {
  abc as "abc"; // okay
  abc as "def" // error "abc" ~> "def"
  _ as "abc" as typeof abc; // okay
  _ as "def" as typeof abc; // error "def" ~> "abc"
}

function test_number() {
  one as 1; // okay
  one as 2 // error 1 ~> 2
  _ as 1 as typeof one; // okay
  _ as 2 as typeof one; // error 2 ~> 1
}

function test_boolean() {
  tru as true; // okay
  tru as false // error true ~> false
  _ as true as typeof tru; // okay
  _ as false as typeof tru; // error false ~> true
}

function test_bigint() {
  bigOne as 1n; // okay
  bigOne as 2n // error 1n ~> 2n
  _ as 1n as typeof bigOne; // okay
  _ as 2n as typeof bigOne; // error 2n ~> 1n
}

function test_string_ref() {
  abcRef as "abc"; // okay
  abcRef as "def" // error "abc" ~> "def"
  _ as "abc" as typeof abcRef; // okay
  _ as "def" as typeof abcRef; // error "def" ~> "abc"
}

function test_number_ref() {
  oneRef as 1; // okay
  oneRef as 2 // error 1 ~> 2
  _ as 1 as typeof oneRef; // okay
  _ as 2 as typeof oneRef; // error 2 ~> 1
}

function test_boolean_ref() {
  truRef as true; // okay
  truRef as false // error true ~> false
  _ as true as typeof truRef; // okay
  _ as false as typeof truRef; // error false ~> true
}

function test_bigint_ref() {
  bigOneRef as 1n; // okay
  bigOneRef as 2n // error 1n ~> 2n
  _ as 1n as typeof bigOneRef; // okay
  _ as 2n as typeof bigOneRef; // error 2n ~> 1n
}

function test_obj() {
  obj.abc as "abc"; // error string ~> "abc"
  obj.one as 1; // error number ~> 1
  obj.tru as true; // error boolean ~> true
  obj.bigOne as 1n; // error bigint ~> 1n
}

function test_spread() {
  spread.abc as "abc"; // error string ~> "abc"
  spread.one as 1; // error number ~> 1
  spread.tru as true; // error boolean ~> true
  spread.bigOne as 1n; // error bigint ~> 1n
}

function test_obj_refs() {
  objRefs.abc as "abc"; // error string ~> "abc"
  objRefs.abcRef as "abc"; // error string ~> "abc"
  spreadObjRefs.abc as "abc"; // error string ~> "abc"
  spreadObjRefs.abcRef as "abc"; // error string ~> "abc"
}

function test_as_const() {
  asConst.abc as "abc"; // okay
  asConst.abcRef as "abc"; // okay
  asConst.abc as "def"; // error "abc" ~> "def"
  asConst.abcRef as "def"; // error "abc" ~> "def"
  _ as "def" as typeof asConst.abc; // error "def" ~> "abc"
  _ as "def" as typeof asConst.abcRef; // error "def" ~> "abc"
  _ as string as typeof asConst.abc; // error string ~> "abc"
  _ as string as typeof asConst.abcRef; // error string ~> "abc"
}

function test_typeof() {
  _ as "abc" as Abc as "abc"; // okay
  _ as 1 as One as 1; // okay
  _ as true as Tru as true; // okay
  _ as 1n as BigOne as 1n; // okay
}

function test_typeof_obj(x: ObjRefs) {
  x.abc as "abc"; // error string ~> "abc"
  x.abcRef as "abc"; // error string ~> "abc"
}

function test_spread_ref(x: SpreadObjRefs) {
  x.abc as "abc"; // error string ~> "abc"
  x.abcRef as "abc"; // error string ~> "abc"
}

function test_obj_with_typeof_abc(x: ObjWithTypeofAbc) {
  x.f as "abc"; // okay
}

function test_objlit() {
  const obj = { f: one };
  obj as {f: 1}; // TODO error number ~> 1
  obj as {f: number}; // okay
}

function test_objlit_as_const() {
  const o = {f: as_const};
  o as {f: "as_const"}; // okay
}

function test_objlit_nullish() {
  declare var n0: ?1;

  const obj1 = { f: n0 ?? one };
  obj1 as {f: 1}; // TODO error number ~> 1
  obj1 as {f: 2}; // error number ~> 2
  1 as typeof obj1.f; // okay 1 ~> number
  2 as typeof obj1.f; // TODO okay 2 ~> number

  const obj2 = { f: one ?? n0 };
  obj2 as {f: 1}; // TODO error number ~> 1
  obj2 as {f: 2}; // error number ~> 2
  1 as typeof obj2.f; // okay 1 ~> number
  2 as typeof obj2.f; // TODO okay 2 ~> number
}

function test_conditional() {
  declare var cond: boolean;
  const x: 'abc' | 'def' = cond ? abc : def; // okay
  const y = cond ? abc : def;
  ({y} as $ReadOnly<{y: 'abc'|'def'}>); // okay
}

declare function useState<T>(x: T): [T, (y: T) => void];

declare function useStateWithDefault<T = {f:1|2}>(x: T): [T, (y: T) => void];

function test_useState_1() {
  const [o, set] = useState({f: one});
  set({f: 1}); // okay
  set({f: 2}); // TODO okay
}

function test_useState_2() {
  const [o, set] = useState<{f:1|2}>({f: one}); // okay
  set({f: 1}); // okay
  set({f: 2}); // okay
  set({f: 3}); // error 3 ~> 1|2
}

function test_useState_4() {
  const [n_, set] = useState(one);
  n_ as 1; // TODO error number ~> 1
  set(2); // TODO okay
}

function test_useState_5() {
  declare var x: typeof one;
  const [x_, set] = useState(x);
  x_ as 1; // okay
  set(2); // error 2 ~> 1
}

function test_useState_6() {
  declare var x: typeof one;
  const [o, set] = useState({f: x});
  set({f: 1}); // okay
  set({f: 2}); // error
}

function test_useState_7() {
  declare function useStateWithBound<T: {f:number|string}>(x: T): [T, (y: T) => void];
  const [o, set] = useStateWithBound({f: one});
  set({f: 1}); // okay
  set({f: 2}); // TODO okay
  set({f: "blah"}); // error "blah" ~> number
}

function test_useState_8() {
  declare function useStateWithBound<T: {f:number}>(x: T): [T, (y: T) => void];
  const [o, set] = useStateWithBound({f: one});
  set({f: 1}); // okay
  set({f: 2}); // TODO okay
  set({f: "blah"}); // error "blah" ~> number
}

function test_useState_9() {
  declare function useStateWithBound<T: {f:1|2}>(x: T): [T, (y: T) => void];
  const [o, set] = useStateWithBound({f: one}); // okay (infers 1)
  set({f: "blah"}); // error "blah" ~> 1
  set({f: 1}); // okay
  set({f: 2}); // error 2 ~> 1
  set({f: 3}); // error 3 ~> 1
}

function test_useState_10() {
  const [o, set] = useStateWithDefault({f: one});
  set({f: 1}); // okay
  set({f: 2}); // error
}

function test_useState_11() {
  abc as "abc";
  const [o, set] = useState({f: abc});
  set({f: "abc"}); // okay
  set({f: "blah"}); // TODO okay
}

function test_apply() {
  declare function apply<T>(f: (v: T) => T, v: T): T;
  apply((v) => {
    v as 1; // TODO error number ~> 1
    return v
  }, one);
}

function test_switch_case() {
  declare var x: number;
  switch (x) {
    case one:
      x as 1; // okay
  }
}

function test_eq_refinement() {
  declare var x: number;
  if (x === one) {
    x as 1; // okay
  }
}

function test_nullish_coalesce() {
  declare var n0: ?number;
  const n1 = n0 ?? one;
  n1 as number; // okay
}

function test_computed_prop_1() {
  declare var x: {
    prop1: number,
    prop2: number,
    prop3: () => void,
  };
  x[prop1] = 1; // okay
  x[prop2]; // okay
  x?.[prop2]; // okay
  x[prop3](); // okay
  x?.[prop3](); // okay
}

function test_computed_prop_2() {
  const obj = {
    [abc]: 1,
    [def]: "", // okay
  };
}

function test_sent_refinement() {
  declare var x: {tag: 'abc'; hi: 'hi'} | {tag: 'bye'};
  if (x.tag === abc) {
      x.hi as string; // okay
  }
}

function test_logical() {
  const x1 = one || "hello";
  x1 as 1; // okay
  let y1 = x1;
  y1 = 5; // TODO okay

  // use hint to preserve precise type
  function fn1(): 1 { return one || ""; } // okay

  // generalize at return
  function fn2() { return one || ""; }
  let x2 = fn2();
  x2 = 5; // TODO okay
  x2 as 5; // TODO error number ~> 5
}

function test_hint_passes_through_arrow() {
  declare function foo<T>(x: () => T): T;
  foo(() => abc) as 'abc';
  foo(() => abc) as 'def'; // error "abc" ~> "def"
}

function test_hint_passes_through_array() {
  declare function foo<T>(x: Array<T>): T;
  foo([abc]) as 'abc';
  foo([abc]) as 'def'; // error "abc" ~> "def"
}

function test_pattern_match() {
  declare var n: number;
  const m = match (n) {
    1 => abc,
    _ => def,
  };
  m as 'abc' | 'def'; // okay
  m as 'abc'; // error 'def' ~> 'abc'
  m as 'def'; // error 'abc' ~> 'def'
}

function test_assign() {
  const x = abc;
  x as 'abc'; // okay
  x as 'def'; // TODO error 'abc' ~> 'def'

  const o1 = {f: x};
  o1 as {f: 'abc'}; // error string ~> 'abc'

  const o2: {f: 'abc'} = {f: x}; // okay
  const o3: {f: string} = {f: x}; // okay
}

function test_reduce() {
  declare var arr: Array<void>;
  const x1 = arr.reduce((acc, _) => acc, [0]);
  x1[0] = 1; // okay x1 inferred as Array<number>
  x1[0] = "a"; // error string ~> number

  const x2 = arr.reduce((acc, _) => acc, [one]);
  x2[0] = 42; // TODO okay x2 inferred as Array<number>
  x2[0] = "a"; // error string ~> number

  const x3: Array<0> = arr.reduce((acc, _) => acc, [0]); // okay
  const x4: Array<1> = arr.reduce((acc, _) => acc, [one]); // okay
}
