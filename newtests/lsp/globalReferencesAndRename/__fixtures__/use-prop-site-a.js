// @flow

import type {Foo} from './prop-def';

const foo: Foo = {bar: 'baz'};
foo.bar;

function MyComponent({bar}: Foo) {
  return <div />;
}
<MyComponent bar="baz" />;
