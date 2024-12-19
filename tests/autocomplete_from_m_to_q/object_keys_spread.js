//@flow

declare var f : {|
  a: number,
  b: number,
  c: number,
  d: number,
  e: number,
|} => void;

const before = {
  a: 1,
  b: 2,
};

const after = {
  d: 1,
  e: 2,
};

// Should omit `b` and keys of `after` (`d`, `e`): suggesting `a` and `c`
f({...before,  , b: 2, ...after});
//            ^
