'use strict';

const Immutable = require('immutable');

const RecordWithDefaultsBase = Immutable.Record({ // ERROR
  label: 'default',
  note: undefined,
});

class RecordWithDefaults extends RecordWithDefaultsBase {}

export default RecordWithDefaults;
