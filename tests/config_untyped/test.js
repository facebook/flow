// @flow

const typed = require('./typed/foo');
const untyped = require('./untyped/foo');

typed as number; // error: string ~> number
untyped as number; // no error, `untyped` is `any`

const nonexistent = require('./untyped/bogus'); // error, missing module
