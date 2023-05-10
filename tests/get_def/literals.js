// @flow

"m#Foo";
// ^

"foo";
// ^

`foo`;
// ^

declare var bar: string;
`foo ${bar}`;
//      ^

declare function foobar(parts: TaggedTemplateLiteralArray): void;
foobar`foo`;
//      ^

foobar`foo`;
// ^

123456;
// ^

false;
// ^

null;
// ^

123456n;
// ^

/foobar/;
// ^
