// @flow

let data = require('./package/index.json');
(data.foo: void); // error, should be object literal
(data.foo.bar: void); // error, should be boolean
(data.abc: boolean); // error, should be ?string

let data2 = require('./package');
(data2.baz: void); // error, should be string

let data3 = require('./package2');
(data3.foo: void); // error, should be number (not string! index.js wins)

let data4 = require('./json_array');
(data4: Array<number>);
(data4: void); // error, should be Array<number>
