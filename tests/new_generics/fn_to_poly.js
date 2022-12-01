//@flow

function foo<T>(x: T): T {
  return x;
}
foo.bar = 'foo';
type Foo = {
  <T>(T): T,
  bar: string,
};

(foo: Foo);

function f(x: mixed): empty {
  return f(x);
}

(f: <T>(T) => T);

function g<T>(x: T): T {
  return x;
}

(g: empty => mixed);
