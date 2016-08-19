/* @flow */

var x : {'123': string, bar: string} = {'123': 'val', bar: 'bar'};
(x.foo : string);     // error, key doesn't exist
(x['foo'] : string);  // error, key doesn't exist
(x[345] : string);  // error, key doesn't exist
(x[123] : boolean);   // error, string !~> boolean
(x.bar: boolean);     // error, string !~> boolean
(x['123'] : boolean); // error, string !~> boolean
x['123'] = false;     // error, boolean !~> string
x[123] = false;       // error, boolean !~> string
x['foo'+'bar'] = 'derp'; // ok since we can't tell
x[123] = 'foo'; // ok
(x[`foo`]: string);   // error, key doesn't exist

var y : {foo: string} = {foo: 'bar'};
y['foo'] = 123; // error, number !~> string
y['bar'] = 'abc'; // error, property not found

(y['hasOwnProperty']: string); // error, prototype method is not a string
