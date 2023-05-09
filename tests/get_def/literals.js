// @flow

"m#Foo";
// ^

"foo";
// ^

`foo`;
// ^

declare function f(parts: Array<string>): void;
f`foo`;
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
