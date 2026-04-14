'use strict';

const Immutable = require('immutable');

const RecordModelBase = Immutable.Record({ // ERROR
  count: 0,
  label: undefined,
  tags: undefined,
});

class RecordModel extends RecordModelBase {}

export default RecordModel;
