/**
 * @flow
 */

// Previously we represented Function as (...rest: any) => any
// This means the following wouldn't pass, because that arrow function
// can only be called with 3 arguments.
var a: Function = (a, b, c) => 123;

var b: Function = function(a: number, b: number): number { return a + b; };

class C {}

var c: Function = C;

function good(x: Function, MyThing: Function): number {
  var o: Object = x; // Function is an Object
  x.foo = 123;
  x['foo'] = 456;
  x();
  <MyThing />;
  var {...something} = x;
  Object.assign(x, {hi: 'there'});
  Object.keys(x);
  return x.bar + x['bar'] + x.lala();
}

function bad(x: Function, y: Object): void {
  var a: number = x; // Error
  var b: string = x; // Error
  var c: Function = y; // Object is not a Function
}

let tests = [
  function(y: () => void) {
    function x() {}
    (x.length: void); // error, it's a number
    (y.length: void); // error, it's a number

    (x.name: void); // error, it's a string
    (y.name: void); // error, it's a string
  },

  function(y: () => void) {
    function x() {}
    x.length = 'foo'; // error, not writable
    y.length = 'foo'; // error, not writable

    x.name = 123; // error, not writable
    y.name = 123; // error, not writable
  },
];

// `Function` types can be bound (resulting in a `Function` type)
var d: Function = () => 1;
var e = (d.bind(1): Function)();
(e: number);
(e: string);
