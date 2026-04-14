'use strict';

const Immutable = require('immutable');

const GenericRecordBase = Immutable.Record({ // ERROR
  foo: undefined,
  bar: undefined,
  baz: undefined,
  qux: undefined,
  fn: undefined,
});

class GenericRecord<T> extends GenericRecordBase<T> {} // ERROR

export default GenericRecord;
