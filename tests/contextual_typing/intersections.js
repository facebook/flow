function test1() {
  declare function foo(tag: 'boolean', cb: (x: boolean) => void): void;
  declare function foo(tag: 'str', cb: (x: string) => void): void;
  declare function foo(tag: 'num', cb: (x: number) => void): void;

  foo('boolean', (x) => { (x: boolean); }); // okay
  foo('str', (x) => { (x: number); }); // error
  foo('a', (x) => { (x: number); }); // error
}

function test2() {
  type B = (tag: 'boolean', cb: (x: boolean) => void) => void;
  type S = (tag: 'str', cb: (x: string) => void) => void;
  type N = (tag: 'num', cb: (x: number) => void) => void;
  type O = (tag: 'obj', cb: (x: {}) => void) => void;

  declare var foo: B & (S & (N & O));
  foo('boolean', (x) => { (x: boolean); }); // okay
  foo('str', (x) => { (x: string); }); // okay
  foo('obj', (x) => { (x: string); }); // error

  declare var bar: (B & S) & (N & O);
  bar('boolean', (x) => { (x: boolean); }); // okay
  bar('str', (x) => { (x: string); }); // okay
  bar('obj', (x) => { (x: string); }); // error

  declare var bak: (B & S) & (N & B);
  bak('boolean', (x) => { (x: boolean); }); // ok
  bak('boolean', (x) => { (x: string); }); // error in cast (ideally would pick the right overload)
}

function test3() {
  declare function foo(x: number, f: (number) => void): void;
  declare function foo(x: string, f: (string) => void): void;

  declare var x: number | string;
  foo(x, y => {}); // error missing annotation for y
}

function test4() {
  [[1]].filter(([n], i: number) => (n: number) > 0); // okay
}

function test5() {
  ["a"].reduce((a, b, i) => a + b); // okay
  ["a"].reduce((a, b, i) => a + b, ''); // okay
}

function test6() {
  declare var array: ?Array<string>;
  (array || []).reduce((acc, item) => acc, {}); // okay
  (array ?? []).reduce((acc, item) => acc, {}); // okay
}

function test7() {
  declare function arrayFrom<A>(iter: $ReadOnlyArray<A>): Array<A>;
  declare function arrayFrom(arrayLike: interface { length: number }): Array<void>;

  declare var tags: ?$ReadOnlyArray<string>;
  const tagsList = arrayFrom(tags ?? []);
  tagsList.forEach(tag => (tag: number)); // error: string ~> number
}

function test8() {
  declare function overload(string, (string) => void): void;
  declare function overload(number, (number) => void): void;

  declare var foo: ?string;
  overload(foo || "", (s) => {}); // okay
  overload(foo || 42, (s) => {}); // error because we can't resolve overload
}
