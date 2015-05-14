/* @flow */

var x : {'123': string} = {'123': 'bar'};
(x['foo'] : string);  // error, key doesn't exist
(x[123] : boolean);   // TODO: use the number's value to error here
(x['123'] : boolean); // error, string !~> boolean
x['123'] = false;     // error, boolean !~> string
x[123] = false;       // TODO: use the number's value to error here
x['foo'+'bar'] = 'derp'; // error, key "foobar" doesn't exist
