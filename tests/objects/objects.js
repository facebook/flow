/* @flow */

var x : {'123': string, bar: string} = {'123': 'val', bar: 'bar'};
(x.foo : string);     // error, key doesn't exist
(x['foo'] : string);  // error, key doesn't exist
(x[123] : boolean);   // error, string !~> boolean
(x.bar: boolean);     // error, string !~> boolean
(x['123'] : boolean); // error, string !~> boolean
x['123'] = false;     // error, boolean !~> string
x[123] = false;       // error, boolean !~> string
x['foo'+'bar'] = 'derp'; // ok since we can't tell
(x[`foo`]: string);   // error, key doesn't exist

var y : {foo: string} = {foo: 'bar'};
y['foo'] = 123; // error, number !~> string
y['bar'] = 'abc'; // error, property not found

(y['hasOwnProperty']: string); // error, prototype method is not a string

// ok because properties named 'constructor' are only banned on classes
var has_constructor_field : {constructor: string} = {constructor: 'foo'}

var z = {123: 'A', '123': 3};
(z[123] : number);
(z['123'] : number);
(z[123] : string); // error, number !~> string
(z['123'] : string); // error, number !~> string
