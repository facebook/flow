// @flow

type Props = {
  name: string,
  age: number
};

type DefaultProps = { age: number };

declare opaque type O1;
declare opaque type O2;

// $Keys<T> TODO this is not an EvalT
const countries = {
  US: "United States",
  IT: "Italy",
  FR: "France"
};

type Country = $Keys<typeof countries>;
//   ^

type KeysPoly<K> = $Keys<K>;
//   ^

// $Values<T>
type Prop$Values = $Values<Props>;
//   ^

const frozenObject = Object.freeze({ A: "a", B: "b" });
type ValuesOfFrozenObject = $Values<typeof frozenObject>;
//   ^

type ValuesPoly<X> = $Values<X>;
//   ^

type ValuesPolyBound<X: { f: number }> = $Values<X>;
//   ^

// $ReadOnly<T>
type ReadOnlyObj = $ReadOnly<{
//   ^
  key: any
}>;

// $Exact<T>
// see exact.js

// Indexed Access
type ElementTypeProps = Props["name"];
//   ^
type ElementTypePropsPoly<K> = Props[K];
//   ^
type ElementTypePropsPolyBounded<K: "name" | "age"> = Props[K];
//   ^
type ElementTypePropsPolyBoundedEmpty<K: "phone"> = Props[K];
//   ^

// $NonMaybeType<T>
type NonMaybeTypeNumber = $NonMaybeType<?number>;
//   ^
type NonMaybeTypeAbstract<X> = $NonMaybeType<X>;
//   ^

// mapped types
type MappedTypeProps = {[K in keyof Props]: Array<Props[K]>}
//   ^

type MappedTypePoly<X, Y> = {[K in keyof { a: X, b?: Y }]: Array<{ a: X, b?: Y }[K]>};
//   ^

type FnObj = { getName: () => string, getAge: () => number };
type MappedTypeFnReturnTypes = {[K in keyof FnObj]: { k: K, v: FnObj[K] extends () => infer V ? V : empty } };
//   ^

// conditional types
type ExtractPropType = <T>({ prop: T }) => T;
type PropObj = { prop: number };
type ConditionalExtractPropType = PropObj extends { prop: infer T } ? T : empty;
//   ^

type NestedObj = {|
  +status: ?number,
  +data: ?$ReadOnlyArray<{|
    +foo: ?{|
      +bar: number
    |}
  |}>
|};

// If you wanted to extract the type for `bar`, you could use conditional type:
type ConditionalNestedObjType = NestedObj extends {
//   ^
  +data: ?$ReadOnlyArray<{
    +foo: ?{
      +bar: ?infer T
    }
  }>
} ? T : empty;

type ConditionalIdPoly<R> = R extends infer N ? N : empty;
//   ^

type PropObjPoly<P> = { prop: P };
type ConditionalExtractPropTypePoly<P> = PropObjPoly<P> extends { prop: infer T } ? T : empty;
//   ^

// $Exports<T>
type ExportsM = $Exports<"lib_m">;
//   ^

// Multi-params (ordering)
declare function right_order<T: {}, K: T>(): T[K];
//               ^
declare function wrong_order<K: T, T: {}>(): T[K];
//               ^

// Recursive
type RecursiveTypeDestructor = {|
//   ^
  f: {|
    g: RecursiveTypeDestructor['f']
  |}
|};

type RecursiveTypeDestructorPoly<X> = {|
//   ^
  f: {| h: RecursiveTypeDestructorPoly<X>['f'] |} | X // TODO
|};

// Nested
type $Pick<O: {}, K: $Keys<O>> = $NonMaybeType<O>[K];
//   ^

// ReadOnly+destructuring

function f({ x }: { x: ReadOnlyObj, ... }) {}
//           ^

// Non-evaluated due to alias
function non_evaluated() {
  type T = $ReadOnly<{
    prop: R,
    ...
  }>;

  type R = $ReadOnly<{
    ...T,
    type: 'blah',
    ...
  }>;

  declare var obj: R;

  const spread = {...obj};
//      ^
// Only show '{prop: R, type: "blah", ...}'. Do not expand R.
}


// TODO
// React.ElementConfigType
// React.ElementRefType
