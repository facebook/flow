// @flow
type Foo = {| +foo: ?string |};
const test = (bar: Foo): Foo => ({ foo: 'foo', ...bar});
