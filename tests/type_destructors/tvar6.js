type T = {a: ?{b: ?{x: string, y: number}}};

type B = NonNullable<T['a']?.['b']>;
const b: B = {x: 'hi', y: 3}; // no error

type O1 = Omit<B, 'y'>;
const obj1: O1 = {x: 'hi'}; // no error

type O2 = Omit<NonNullable<T['a']?.['b']>, 'y'>;
const obj2: O2 = {x: 'hi'}; // no error
