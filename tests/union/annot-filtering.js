export type T = {
  f: boolean,
};

declare var xs: T | void;

const ys: T | typeof undefined = xs;

// The conditional should filter out 'typeof undefined'
if (ys != undefined) {
  ys.f = true; // okay
}
