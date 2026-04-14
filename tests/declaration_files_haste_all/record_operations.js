import type Immutable from 'immutable';

import RecordModel from './RecordModel';
import RecordWithDefaults from './RecordWithDefaults';

declare const tags: Immutable.List<string>;

const record = new RecordModel({
  count: 1,
  label: 'ok',
  tags,
});

new RecordModel({
  count: 1,
  label: 'ok',
}); // ERROR

record.set('count', 'bad'); // ERROR
record.set('count', 2);
record.update('label', value => value + '!');
record.withMutations(mutable => {
  mutable.label = 'changed';
  mutable.set('count', 3);
  mutable.label = 42; // ERROR
});

const defaults = new RecordWithDefaults();
defaults.get('label') as string;

const overridden = new RecordWithDefaults({label: 'other'});
overridden.get('note') as ?string;

const merged = overridden.merge({note: 'ok'});
merged.get('note') as ?string;

overridden.set('label', null); // ERROR
