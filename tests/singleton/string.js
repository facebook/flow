/* @flow */

type NoSpaces = "foobar"
("foobar": NoSpaces);

type HasSpaces = "foo bar"
("foo bar": HasSpaces);

function constLiteral() {
  const foo = 'str';
  const foo_check: typeof foo = 'bar';
  const mutable_foo = {
    foo,
    method() {
      mutable_foo.foo = 2;
    }
  };
  (mutable_foo.foo: string); // error
}
