//@flow

const {lit, long_lit} = require('./long_string_lit');

lit as 'aaaaaaaaaaaaaaaaaaaa'; // ok
long_lit as 'aaaaaaaaaaaaaaaaaaaaa'; // error
