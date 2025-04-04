// @flow

export const abc = "abc";
export const def = "def";
export const one = 1;
export const tru = true;
export const bigOne = 1n;
export const abcRef = abc;
export const oneRef = one;
export const truRef = tru;
export const bigOneRef = bigOne;
export const as_const = "as_const" as const;
export const prop1 = 'prop1';
export const prop2 = 'prop2';
export const prop3 = 'prop3';
export const obj = { abc: "abc", one: 1, tru: true, bigOne: 1n };
export const spread = { ...obj };
export const objRefs = { abc, abcRef };
export const spreadObjRefs = { ...objRefs };
export const asConst = { abc, abcRef } as const;
export type Abc = typeof abc;
export type One = typeof one;
export type Tru = typeof tru;
export type BigOne = typeof bigOne;
export type ObjRefs = typeof objRefs;
export type SpreadObjRefs = typeof spreadObjRefs;
export type ObjWithTypeofAbc = { f: typeof abc };

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
  obj as {f: 1}; // error number ~> 1
  obj as {f: number}; // okay
}

function test_objlit_as_const() {
  const o = {f: as_const};
  o as {f: "as_const"}; // okay
}

function test_objlit_nullish() {
  declare var n0: ?1;

  const obj1 = { f: n0 ?? one };
  obj1 as {f: 1}; // error number ~> 1
  obj1 as {f: 2}; // error number ~> 2
  1 as typeof obj1.f; // okay 1 ~> number
  2 as typeof obj1.f; // okay 2 ~> number

  const obj2 = { f: one ?? n0 };
  obj2 as {f: 1}; // error number ~> 1
  obj2 as {f: 2}; // error number ~> 2
  1 as typeof obj2.f; // okay 1 ~> number
  2 as typeof obj2.f; // okay 2 ~> number
}

function test_conditional_1() {
  declare var cond: boolean;
  const x: 'abc'|'def' = cond ? abc : def; // okay
  const y = cond ? abc : def;
  ({y} as $ReadOnly<{y: 'abc'|'def'}>); // okay
}

function test_conditional_2() {
  declare var cond: boolean;
  declare var foo: () => "foo";

  const x1 = cond ? "a" : "b";
  const o1 = {f: x1};
  o1.f as "a" | "b"; // error string ~> "a" | "b"

  const x2 = cond ? foo() : "a";
  const o2 = {f: x2};
  o2.f as "foo" | "a"; // error string ~> "foo" | "a"

  const x3 = cond ? {a: 'a'} : foo();
  const o3 = {f: x3};
  o3.f as {a: 'a'} | "foo"; // error 'a's contribution is string
}

declare function useState<T>(x: T): [T, (y: T) => void];

declare function useStateWithDefault<T = {f:1|2}>(x: T): [T, (y: T) => void];

function test_useState_1() {
  const [o, set] = useState({f: one});
  set({f: 1}); // okay
  set({f: 2}); // okay
}

function test_useState_2() {
  const [o, set] = useState<{f:1|2}>({f: one}); // okay
  set({f: 1}); // okay
  set({f: 2}); // okay
  set({f: 3}); // error 3 ~> 1|2
}

function test_useState_4() {
  const [n_, set] = useState(one);
  n_ as 1; // error number ~> 1
  set(2); // okay
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
  set({f: 2}); // TODO(?) okay the type of `one` is checked against a general bound
  set({f: "blah"}); // error
}

function test_useState_8() {
  declare function useStateWithBound<T: {f:number}>(x: T): [T, (y: T) => void];
  const [o, set] = useStateWithBound({f: one});
  set({f: 1}); // okay
  set({f: 2}); // okay
  set({f: "blah"}); // error "blah" ~> number
}

function test_useState_9() {
  declare function useStateWithBound<T: {f:1|2}>(x: T): [T, (y: T) => void];
  const [o, set] = useStateWithBound({f: one}); // infer specific type due to check against bound
  set({f: "blah"}); // error "blah" ~> 1
  set({f: 1}); // okay
  set({f: 2}); // error 2 ~> 1
  set({f: 3}); // error 3 ~> 1
}

function test_useState_10() {
  const [o, set] = useStateWithDefault({f: one}); // infers general type
  set({f: 1}); // okay
  set({f: 2}); // okay
}

function test_useState_11() {
  abc as "abc";
  const [o, set] = useState({f: abc});
  set({f: "abc"}); // okay
  set({f: "blah"}); // okay
}

function test_apply() {
  declare function apply<T>(f: (v: T) => T, v: T): T;
  apply((v) => {
    v as 1; // error number ~> 1
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
  y1 = 5; // okay

  // use hint to preserve precise type
  function fn1(): 1 { return one || ""; } // okay

  // generalize at return
  function fn2() { return one || ""; }
  let x2 = fn2();
  x2 = 5; // okay
  x2 as 5; // TODO(?)  error number ~> 5 (for now we use the local type)

  declare var maybeOne: ?1;
  const two = 2;

  const nullish = maybeOne ?? two;
  nullish as 1 | 2; // okay

  const or_ = maybeOne || two;
  or_ as 1 | 2; // okay

  const and_ = maybeOne && two;
  and_ as ?(1 | 2); // okay
}

function test_logical_literals() {
  const x1 = 42 || "hello";
  x1 as 42; // okay
  let y1 = x1;
  y1 = 5; // okay

  // use hint to preserve precise type
  function fn1(): 42 { return 42 || ""; } // okay

  // generalize at return
  function fn2() { return 42 || ""; }
  let x2 = fn2();
  x2 = 5; // okay
  x2 as 5; // TODO(?) error number ~> 5
}

function test_synthesis_literals_1() {
  declare function optional<P>({p: P}): P;
  let o = optional({p: 3});
  o = 1; // ok
}

function test_synthesis_literals_2() {
  declare function optional<P>({p: P}): P;
  const o = optional({p: {bar: 3}}); // ok
  o.bar = 1; // ok
}

function test_synthesis_literals_3() {
  declare var cp: {bar: number};
  declare function optional<P>($ReadOnly<{|cp: P, ...P|}>): P;
  const o = optional({cp, bar: 3}); // ok
}

function test_hint_passes_through_arrow() {
  declare function foo<T>(x: () => T): T;
  foo(() => abc) as 'abc'; // okay - contextual type is used to infer 'abc'
  foo(() => abc) as 'def'; // error "abc" ~> "def"
}

function test_hint_passes_through_array() {
  declare function foo<T>(x: Array<T>): T;
  foo([abc]) as 'abc'; // okay - contextual type is used to infer 'abc'
  foo([abc]) as 'def'; // error "abc" ~> "def"
}

function test_pattern_match() {
  declare var n: number;
  const m1 = match (n) {
    1 => 'a',
    _ => 'b',
  };
  m1 as 'a' | 'b'; // okay
  m1 as 'a'; // error 'b' ~> 'a'
  m1 as 'b'; // error 'a' ~> 'b'

  const m2 = match (n) {
    1 => abc,
    _ => def,
  };
  m2 as 'abc' | 'def'; // okay
  m2 as 'abc'; // error 'def' ~> 'abc'
  m2 as 'def'; // error 'abc' ~> 'def'
}

function test_assign() {
  const x = abc;
  x as 'abc'; // okay
  x as 'def'; // error 'abc' ~> 'def'

  const o1 = {f: x};
  o1 as {f: 'abc'}; // error string ~> 'abc'

  const o2: {f: 'abc'} = {f: x}; // okay
  const o3: {f: string} = {f: x}; // okay
}

function test_class_bound() {
  class C {
    set<K: 'a'>(k: K) {}
  }

  const a = 'a';
  const c = new C();
  c.set(a); // okay
  c.set('a'); // okay
  c.set('b'); // error "b" ~> "a"
}

function test_reduce() {
  declare var arr: Array<void>;
  const x1 = arr.reduce((acc, _) => acc, [0]);
  x1[0] = 1; // okay x1 inferred as Array<number>
  x1[0] = "a"; // error string ~> number

  const x2 = arr.reduce((acc, _) => acc, [one]);
  x2[0] = 42; // okay x2 inferred as Array<number>
  x2[0] = "a"; // error string ~> number

  const x3: Array<0> = arr.reduce((acc, _) => acc, [0]); // okay
  const x4: Array<1> = arr.reduce((acc, _) => acc, [one]); // okay
}

function test_logical_instantiation() {
  declare var zerOrOne: 0|1;
  const x = zerOrOne || 2;

  const [arr, _] = useState([x]);
  arr[0] as 1|2; // error number ~> 1|2
}

function test_destructure_computed() {
  const PROP = 'prop';
  const {[PROP]: one} =  {prop: 1};
  one as 1; // error number ~> 1
  one as 2; // error number ~> 2
}

function test_computed_prop_hint_1() {
  type Name = 'a'| 'b' | 'c';
  const KeyName = 'a';
  ({[KeyName]: KeyName} as $ReadOnly<{[Name]: 'a'}>); // okay
  ({[KeyName]: KeyName} as $ReadOnly<{[Name]: 'b'}>); // error 'a' ~> 'b'
}

function test_synthesis_produced_uncacheable_result() {
  declare function foo<X: "a" | "b">(x: X, cb: (x: X) => void): void;
  const k = "a";
  foo(k, x => { // okay k is "a"
    x as string; // okay
    x as "a"; // okay
    x as number; // error string ~> number
  });
}

function test_computed_prop_hint_2() {
  const A = 'a';
  const B = 'b';

  type T = {
    a: number,
    b: string,
  };

  declare function foo<X>(): X;
  const x: T = {
    [A]: foo(), // okay - inferred number as return
    [B]: foo(), // okay - inferred string as return
  };
}

function test_conditional_pass_flag() {
  const prop = 'prop';
  const obj = { prop: {} };

  declare var cond: boolean;
  obj[cond ? prop : 'prop']; // okay access of prop
  obj[cond ? 'prop' : prop]; // okay access of prop
}

function test_generalize_function_literal() {
  type GenericFnType<TReturn> = () => TReturn;
  declare function memo<TReturn, Fn: GenericFnType<TReturn>>(f: Fn): Fn;
  const memoized = memo(() => ({prop: 0})); // should generalize to `() => {prop: number}`

  const obj = memoized();
  obj.prop = 0; // okay
  obj.prop = 1; // okay
  obj.prop = "a"; // error string ~> number
}

function test_async() {
  class AsyncSelector<T>  {
    constructor(getState: () => T) {}
  }

  const x = new AsyncSelector(async () => ({title: ''}));
  x as AsyncSelector<Promise<{title: string}>>; // okay '' has generalized to string
}

function test_logical_nullish() {
  declare const x: ?('a' | 'b');
  declare var arr: Array<void>;
  const mappedArr = arr.map(_ => ({
    prop: x ?? 'c',
  }));
  mappedArr[0].prop = 'd'; // okay - 'c' causes generalization to string
}
function test_array_elem_union() {
  declare function foo<T>(create: T): T;
  const strings = foo(['a', 'b']);
  strings.push("c"); // okay
}

function test_generalize_under_intersection() {
  declare function foo<V>(
    v: $Exact<V>,
    cb: (data: $Exact<V>) => void,
  ): void;

  foo(
    {f:1},
    (x: {f: number}) => {}, // okay, type of `1` should generalize
  );
}
