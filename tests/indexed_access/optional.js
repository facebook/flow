type Obj = {|
  foo?: {|
    bar: number,
  |},
|};


(1: Obj['foo']?.['xxx']); // Error - wrong prop

type X = Obj['foo']['bar']; // Error - access `bar` on undefined
(1: X);

type T = Obj['foo']?.['bar'];
(1: T); // OK
(true: T); // Error - wrong type

type Z = {|c: number|};
type Y = {|a: ?{|b: Z, opt_b: ?Z|}|};

(1: Y["a"]["b"]?.["c"]); // Error - access `b` on undefined
(1: Y["a"]?.["opt_b"]["c"]); // Error - access `c` on undefined
(1: (Y["a"]?.["b"])["c"]); // Error - access `c` on undefined
(1: Y["a"]?.["xxx"]); // Error - non-existent prop
(true: Z?.['c']); // Error - wrong type

(1: Y["a"]?.["b"]["c"]); // OK
(undefined: Y["a"]?.["b"]["c"]); // OK
(true: Y["a"]?.["b"]["c"]); // Error - wrong type

(1: Y["a"]?.["b"]?.["c"]); // OK
(undefined: Y["a"]?.["b"]?.["c"]); // OK
(true: Y["a"]?.["b"]?.["c"]); // Error - wrong type

(1: Y["a"]?.["opt_b"]?.["c"]); // OK
(undefined: Y["a"]?.["opt_b"]?.["c"]); // OK
(true: Y["a"]?.["opt_b"]?.["c"]); // Error - wrong type

(1: Z?.['c']); // OK
(true: Z?.['c']); // Error - wrong type
(undefined: Z?.['c']); // OK - this behaviour does not match optional chaining at the value level, if in the future this changes to be an error that would be fine

type U = {|a: void | {|b: Z, opt_b: null | Z|}|};

(1: U["a"]?.["b"]["c"]); // OK
(undefined: U["a"]?.["b"]["c"]); // OK
(true: U["a"]?.["b"]["c"]); // Error - wrong type

(1: U["a"]?.["b"]?.["c"]); // OK
(undefined: U["a"]?.["b"]?.["c"]); // OK
(true: U["a"]?.["b"]?.["c"]); // Error - wrong type

(1: U["a"]?.["opt_b"]?.["c"]); // OK
(undefined: U["a"]?.["opt_b"]?.["c"]); // OK
(true: U["a"]?.["opt_b"]?.["c"]); // Error - wrong type

export type MaybeZ = ?Z;
(1: MaybeZ?.['c']); // OK
(undefined: MaybeZ?.['c']); // OK
(true: MaybeZ?.['c']); // Error - wrong type

type VoidZ = void | Z;
(1: VoidZ?.['c']); // OK
(undefined: VoidZ?.['c']); // OK
(true: VoidZ?.['c']); // Error - wrong type

(undefined: null?.['c']); // OK
(undefined: void?.['c']); // OK
(1: void?.['c']); // Error

(undefined: empty?.['x']); // OK
(1: empty?.['x']); // Error

function f<K: string, T: ?{+[K]: mixed}>(t: T, k: K): T?.[K] {
  return t?.[k];
}

declare var z: Z;
(f(z, 'c'): number | void); // OK
(f(z, 'c'): true); // Error - wrong type
f(z, 'xxx'); // Error - non-existent prop

declare var mz: MaybeZ;
(f(mz, 'c'): number | void); // OK
(f(mz, 'c'): true); // Error - wrong type
f(mz, 'xxx'); // Error - non-existent prop

type Before = Later?.['a'];
('s': Before); // Error
(0: Before); // OK
(undefined: Before); // OK
type Later = ?{a: number};

type O = ?{
  [number]: boolean;
  foo: string;
  bar: number;
  nested: {
    goop: number,
  },
}
(1: O?.['bar']); // OK
(true: O?.[number]); // OK
('xx': O?.['bar']); // Error
(1: O?.['nested']['goop']); // OK

declare class Cx {
  [number]: boolean;
  foo: string;
  bar: number;
  nested: {
    goop: number,
  },
}
type C = ?Cx;
(1: C?.['bar']); // OK
(true: C?.[number]); // OK
('xx': C?.['bar']); // Error
(1: C?.['nested']['goop']); // OK

type NonMaybeNumber = $NonMaybeType<Obj['foo']?.['bar']>;
(1: NonMaybeNumber); // OK
(true: NonMaybeNumber); // Error
(undefined: NonMaybeNumber); // Error
