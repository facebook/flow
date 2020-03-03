// @flow

type F = (x: string) => void;
function g(a: string, f: F, b: number) {}

g(
  'a',
  x => {
    /* here */
  },
  123,
);
