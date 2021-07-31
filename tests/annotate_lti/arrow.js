// @flow

const f = x => x;
f(1)

const f2 = (x, y) => x;
f2(1, 'test')

const ret_val = (x): number => x;

const callback = e => e + 1;
[1, 2, 3].map(callback);

// Should be ignored
[1, 2, 3].map(e => e + 1);

