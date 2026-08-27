// The check is a lint that is on by default, so it can be turned off for a
// region of a file. Nothing here needs a `[lints]` entry in the `.flowconfig`.

type O = {
  m: (this: {x: number, ...}, y: number) => void,
};

declare const o: O;
declare const other: O;

/* flowlint invalid-this-arg:off */
o.m.call(other, 1); // error: lint disabled, `this` incompatibility still reported
/* flowlint invalid-this-arg:error */

o.m.call(other, 1); // error: lint re-enabled

// flowlint-next-line invalid-this-arg:off
o.m.bind(other); // error: lint disabled, `this` incompatibility still reported

o.m.bind(other); // error
