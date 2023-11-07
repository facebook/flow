// @flow
function foo(x: ?string): $NonMaybeType<?string> {
  if (x != null) {
    return x;
  } else return 0; // this should be an error
}

//(foo(): string); // should not be necessary to expose the error above

0 as $NonMaybeType<null>; // error
0 as $NonMaybeType<?number>; // ok
0 as $NonMaybeType<number | null>; // ok
0 as $NonMaybeType<$PropertyType<{p?: number}, 'p'>>; // ok

'str' as $NonMaybeType<mixed>;
0 as $NonMaybeType<mixed>;
null as $NonMaybeType<mixed>;
undefined as $NonMaybeType<mixed>;

type Foo = $NonMaybeType<$PropertyType<Obj, 'foo'>>;

type Enum = 'A' | 'B' | 'C';

type Obj = {foo: ?Enum};

class B {
  type: Foo = 'A';
}

type Node = number | string;

declare class A<T> {
  concat<S, Item: A<S> | S>(items: A<Item>): A<T | S>;
  filter(callbackfn: typeof Boolean): A<$NonMaybeType<T>>;
}

declare function mkA<T>(x: T): A<T>;

declare function bar(): Node;

declare var props: {s: string};

function f(this: {props: typeof props}) {
  let x = this.props.s === 'S' ? mkA(bar()) : mkA();
  let y = x.concat(mkA(bar()));
  let z: A<Node> = y.filter(Boolean); // should not be an error, but unions + generics are broken
}

'hi' as $NonMaybeType<empty>; // Error

type NonMaybeNumber = $NonMaybeType<$Values<{a: number, b: void}>>;
1 as NonMaybeNumber; // OK
true as NonMaybeNumber; // Error
undefined as NonMaybeNumber; // Error
