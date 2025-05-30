// @jsx customJsx

declare var Foo: any;
declare function customJsx<T>(c: any, props: {|bar: '3'|}, children: T): T;

const result = <Foo bar="3">{3}</Foo>; // ok
result as number; // ok
result as string; // error
