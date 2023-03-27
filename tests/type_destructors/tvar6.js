type T = {a: ?{b: ?{x: string, y: number}}};

type B = $NonMaybeType<T['a']?.['b']>;
const b: B = {x: 'hi', y: 3}; // no error

type O1 = $Diff<B, {y: mixed}>;
const obj1: O1 = {x: 'hi'}; // no error

type O2 = $Diff<$NonMaybeType<T['a']?.['b']>, {y: mixed}>;
const obj2: O2 = {x: 'hi'}; // no error
