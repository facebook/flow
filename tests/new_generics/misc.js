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
  (x === y: boolean); // nope, constant-condition error
  (x < x: boolean);
  (x < y: boolean); // nope
}

function c<S: string, X: {[string]: unknown}, Y: Array<number>>(
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

function h<X: [number]>(x: X): {[_K in keyof X]: string} {
  return ['a']; // error, mapped type doesn't have the unsoundness issue
}

// ToStringT
function gn<TType>(jsEnum: {[TType]: string, ...}) {
  (Object.keys(jsEnum): ReadonlyArray<TType>);
}

// KeysT
function gv<
  TFormData: {},
  TValidators: {[K in keyof TFormData]: number},
>(
  data: TFormData,
  validators: TValidators,
): {[K in keyof TFormData]: ?string} {
  return Object.keys(data).reduce( // error: cannot satisfy generic mapped type
    <K: keyof TFormData>(acc: {[K in keyof TFormData]: ?string}, k: K): {[K in keyof TFormData]: ?string} =>
      // $FlowExpectedError[unsafe-object-assign]
      Object.assign(acc, {[k]: validators[k](k, data)}),
    {},
  );
}

// More KeysT
function kt<TKey: keyof {a: 42}>(fieldName: TKey): void {
  if (fieldName) {
    return;
  }
}

// OptionalChain.run
function oc<
  T: Readonly<{id: ?string, ...}>,
  L: ?Readonly<{
    id: ?string,
    nextOneOne: ?{...},
    ...
  }>,
>(
  conversations: ReadonlyArray<T>,
  lookup: {[string]: L, ...},
): ReadonlyArray<T> {
  return conversations.filter(conversation => {
    const {id} = conversation;
    if (id == null) {
      return false;
    }
    return lookup[id]?.nextOneOne == null;
  });
}

// generic of key of generic
const cc = () => <D: {...}, K: keyof D>(key: K, data: D) => {
  const [click, view] = data[key];
};

// generic refined to empty
type ObjectKey = string | number;
type ObjectType<TK, TV> = {[key: TK]: TV, ...};
function ObjectFlip<TK: ObjectKey, TV: ?ObjectKey>(
  obj: ObjectType<TK, TV>,
  key: TK,
): {[string | number]: TK} {
  const flipped: {[string | number]: TK} = {};
  const value: ?TV = obj[key];
  if (value !== null && value !== undefined) {
    flipped[value] = key;
  }
  return flipped;
}

// Type Destructor
type LLETT =
  | {response: {account: {activities: number}}}
  | {response: {account: {}}};

type LLETR = LLETT['response'];
const elementType = <T: LLETR>(data: T): T['account'] =>
  data.account;

const directAccount = <T: LLETR>(data: T, otherData: LLETR): Readonly<T> =>
  otherData;

type t = {a: number} | {v: string};

class C<TConfig: t> {
  _config: TConfig;
  __updateConfig(updater: (config: Readonly<TConfig>) => TConfig) {
    const config = updater(this._config);
  }
}

// Keys
function gejses<TMapKey: string>(
  key: keyof {[TMapKey]: $FlowFixMe, ...},
): null {
  if (key) {
  }
  return null;
}

// Partial
function partial_test<T>(p: Partial<T>): T {
  return p; // error
}
