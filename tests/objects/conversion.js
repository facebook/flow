/* @flow */

Object({foo: 'bar'}) as {foo: string};
Object('123') as String;
Object(123) as Number;
Object(true) as Boolean;
Object(null) as {};
Object(undefined) as {};
Object(void 0) as {};
Object(undefined) as Number; // error

var x = Object(null);
x.foo = 'bar';

var y = Object('123');
y.charAt(0) as string;

var z = Object(123); // error (next line makes this not match any signatures)
z.charAt(0) as string;
