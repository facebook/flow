'use strict';

const Immutable = require('immutable');

const GenericRecordWithDefaultsBase = Immutable.Record({ // ERROR
  foo: 'foo',
  bar: undefined,
});

class GenericRecordWithDefaults extends GenericRecordWithDefaultsBase {}

export default GenericRecordWithDefaults;
