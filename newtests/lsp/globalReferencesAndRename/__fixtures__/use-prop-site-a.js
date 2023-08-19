// @flow

import type {Foo} from './prop-def';

const foo: Foo = {bar: 'baz'};
foo.bar;

function MyComponent({bar}: Foo) {
  bar;
  return <div />;
}
<MyComponent bar="baz" />;

const {bar} = foo;
const baz = bar;
