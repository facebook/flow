// @jsx customJsx

declare var Foo: any;
declare function customJsx(c: any, props: {foo: "a", bar: 42}, children: 42): void;

const foo = "a";
const bar = 42;

const result = [
  <Foo bar={bar} foo={foo}>
    {bar}
  </Foo>,
  <Foo bar={42} foo={'a'}>
    {42}
  </Foo>,
  <Foo bar={43 /* error 43 ~> 42 */} foo={'b' /* error 'b' ~> 'a' */}>
    {43 /* error 43 ~> 42 */}
  </Foo>,
];
