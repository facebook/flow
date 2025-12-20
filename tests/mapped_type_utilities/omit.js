// Unlike Pick, omit is not homomorphic and has no special behavior for
// distributivity
type O = {foo: string, bar: number, baz: number};

type OmitFoo = Omit<O, 'foo'>;
declare const noFoo: OmitFoo;
noFoo.foo; // ERROR
noFoo as {bar: number, baz: number}; // OK

type OmitFooAndBar = Omit<O, 'foo' | 'bar'>;
declare const noFooOrBar: OmitFooAndBar;
noFooOrBar.foo; // ERROR
noFooOrBar.bar; // ERROR
noFooOrBar.baz; // OK
noFooOrBar as {baz: number}; // OK

const o: Omit<{ foo?: string, bar: number; }, "bar"> = {}; // OK

// Interfaces
interface I {
  foo: string;
  bar: number;
  baz: number
}

type OmitInterface = Omit<I, 'foo'>;
{
  declare const noFoo: OmitInterface;
  noFoo as interface {bar: number; baz: number}; // OK
  noFoo.foo; // ERROR: omitted
  noFoo as {bar: number; baz: number, ...}; // ERROR: interface not object
  class K implements OmitInterface {bar: number; baz: number} // OK
}

// Instances
class C {
  foo: string;
  bar: number;
  baz: number
}

type OmitInstance = Omit<C, 'foo'>;
{
  declare const noFoo: OmitInstance;
  noFoo as interface {bar: number; baz: number}; // OK
  noFoo.foo; // ERROR: omitted
  noFoo as {bar: number; baz: number, ...}; // ERROR: interface not object
}

// Omit preserves polarity
{
  declare const x: Omit<{+foo: 1, bar: 2}, 'bar'>;
  x as {+foo: 1}; // OK
  x as {foo: 1}; // ERROR
}

// Omit on inexact inputs results in exact outputs
{
  declare const x: Omit<{foo: 1, bar: 2, ...}, 'bar'>;
  x as {foo: 1}; // OK
}

// Omitting non-existent property
{
  declare const x: Omit<{foo: 1, bar: 2}, 'xxx'>; // OK
}
