/* @flow */

type Foo = {foo: string};
const a: Foo = {foo: ''};
a.foo;

const b = {foo: ''};
b.foo;

{
  const {foo} = a;
  const {foo: bar} = a;
};
{ const {foo} = b; }
{ const [foo] = a; }
{ const foo = a.foo; }
