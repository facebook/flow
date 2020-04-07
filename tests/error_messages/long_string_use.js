//@flow

const { lit, long_lit } = require('./long_string_lit');

(lit     : 'aaaaaaaaaaaaaaaaaaaa'); // ok
(long_lit: 'aaaaaaaaaaaaaaaaaaaaa'); // error
