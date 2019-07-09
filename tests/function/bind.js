// @flow

let tests = [
  function(x: (a: string, b: string) => void) {
    let y = x.bind(x, 'foo');
    y('bar'); // ok
    y(123); // error, number !~> string
  },

  // callable objects
  function(x: { (a: string, b: string): void }) {
    let y = x.bind(x, 'foo');
    y('bar'); // ok
    y(123); // error, number !~> string
  },

  // non-callable objects
  function(x: { a: string }) {
    x.bind(x, 'foo'); // error
  },

  // callable objects with overridden `bind` method
  function(x: { (a: string, b: string): void, bind(a: string): void }) {
    (x.bind('foo'): void); // ok
    (x.bind(123): void); // error, number !~> string
  },

  function(x: (x: number) => void) {
    const appliedFn = Function.apply.bind(x); // ok
    appliedFn(null, ['']); // error
  },


  function(x: (x: number) => void, y: (x: string) => void) {
    const appliedFn = Function.apply.bind(x); // ok
    const reappliedFn = appliedFn.bind(y); // wrong, should error
    reappliedFn(null, ['']); // wrong
  },

  function(x: (x: number) => void) {
    const appliedFn = Function.apply.bind(x, null); // should be ok
    appliedFn(['']); // wrong
  },
];
