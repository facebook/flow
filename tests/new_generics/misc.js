//@flow

function a<X: number>(x: X) {
  // AdderT
  (x + x: X); // nope
  (x + x: number);
  // UnaryMinusT
  (-x: X); // nope
  (-x: number);
  // AssertArithOperandT
  (x * x: X);
  (x * x: number);
  // coercion
  (`blah ${x}`: string);
}

function b<X: number, Y: string>(x: X, y: Y) {
  //EqT, StrictEqT, CompartatorT
  (x == x: boolean);
  (x === x: boolean);
  (x == y: boolean); // nope
  (x === y: boolean);
  (x < x: boolean);
  (x < y: boolean); // nope
}

function c<S: string, X: {[string]: mixed}, Y: Array<number>>(
  s: S,
  x: X,
  y: Y,
) {
  // AssertForInRHST
  for (const _ in x) {
  }
  // AssertBinaryIn[L|R]HST
  s in x;
  42 in y;
}

// ShapeT
function d<X: {a: number}>(x: X): $Shape<{a: number}> {
  return x;
}

// MatchingPropT
function e<X: {a: 'T'} | {a: 'S'}>(x: X) {
  if (x.a === 'T') {
  }
}

//TestPropT
function f<X>(x: Array<X>) {
  if (x[0] != null && x[0].id === 'a') {
    // This seems questionable but models current behavior for mixed
  }
}

// fast path for TupleMap
function h<X: [number]>(x: X): $TupleMap<X, (number) => string> {
  return ['a']; // existing unsoundness
}

// ToStringT
function gn<TType>(jsEnum: {[TType]: string, ...}) {
  (Object.keys(jsEnum): Array<TType>);
}

// KeysT
function gv<
  TFormData: {},
  TValidators: $ObjMap<TFormData, (_: mixed) => number>,
>(
  data: TFormData,
  validators: TValidators,
): $ObjMap<TFormData, (_: mixed) => ?string> {
  return Object.keys(data).reduce(
    <K: $Keys<TFormData>>(acc, k: K) =>
      Object.assign(acc, {[k]: validators[k](k, data)}),
    {},
  );
}

// More KeysT
function kt<TKey: $Keys<{a: 42}>>(fieldName: TKey): void {
  if (fieldName) {
    return;
  }
}

// OptionalChainT
function oc<
  T: $ReadOnly<{id: ?string, ...}>,
  L: ?$ReadOnly<{
    id: ?string,
    nextOneOne: ?{...},
    ...
  }>,
>(
  conversations: $ReadOnlyArray<T>,
  lookup: {[string]: L, ...},
): $ReadOnlyArray<T> {
  return conversations.filter(conversation => {
    const {id} = conversation;
    if (id == null) {
      return false;
    }
    return lookup[id]?.nextOneOne == null;
  });
}

// generic of key of generic
const cc = () => <D: {...}, K: $Keys<D>>(key: K, data: D) => {
  const [click, view] = data[key];
};

// generic refined to empty
type ObjectKey = string | number;
type ObjectType<TK, TV> = {[key: TK]: TV, ...};
function ObjectFlip<TK: ObjectKey, TV: ?ObjectKey>(
  obj: ObjectType<TK, TV>,
  key: TK,
) {
  var flipped = {};
  const value: ?TV = obj[key];
  if (value !== null && value !== undefined) {
    flipped[value] = key;
  }
  return flipped;
}
