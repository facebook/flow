// @flow
function foo(x: ?string): NonNullable<?string> {
  if (x != null) {
    return x;
  } else return 0; // this should be an error
}

//(foo(): string); // should not be necessary to expose the error above

0 as NonNullable<null>; // error
0 as NonNullable<?number>; // ok
0 as NonNullable<number | null>; // ok
0 as NonNullable<{p?: number}['p']>; // ok

'str' as NonNullable<mixed>;
0 as NonNullable<mixed>;
null as NonNullable<mixed>;
undefined as NonNullable<mixed>;

type Foo = NonNullable<Obj['foo']>;

type Enum = 'A' | 'B' | 'C';

type Obj = {foo: ?Enum};

class B {
  type: Foo = 'A';
}

type Node = number | string;

declare class A<T> {
  concat<S, Item: A<S> | S>(items: A<Item>): A<T | S>;
  filter(callbackfn: typeof Boolean): A<NonNullable<T>>;
}

declare function mkA<T>(x: T): A<T>;

declare function bar(): Node;

declare var props: {s: string};

function f(this: {props: typeof props}) {
  let x = this.props.s === 'S' ? mkA(bar()) : mkA();
  let y = x.concat(mkA(bar()));
  let z: A<Node> = y.filter(Boolean); // should not be an error, but unions + generics are broken
}

'hi' as NonNullable<empty>; // Error

type NonMaybeNumber = NonNullable<Values<{a: number, b: void}>>;
1 as NonMaybeNumber; // OK
true as NonMaybeNumber; // Error
undefined as NonMaybeNumber; // Error
