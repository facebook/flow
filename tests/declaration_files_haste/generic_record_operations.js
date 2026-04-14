import GenericRecord from 'GenericRecord';
import GenericRecordWithDefaults from 'GenericRecordWithDefaults';

new GenericRecord(); // ERROR

const record = new GenericRecord<number>({
  foo: new Map() as Map<string, number>,
  bar: 'bar',
  qux: 1,
});

record.get('foo') as Map<string, number>;
record.get('bar') as string;
record.get('qux') as number;

record.set('qux', 'bad'); // ERROR
record.set('qux', 2);
record.update('bar', value => value + '!');
record.update('qux', value => value + 1);
record.withMutations(mutable => {
  mutable.bar = 'changed';
  mutable.set('qux', 3);
});

const defaults = new GenericRecordWithDefaults();
defaults.get('foo') as string;

const overridden = new GenericRecordWithDefaults({foo: 'other'});
const merged = overridden.merge({bar: 'ok'});

merged.get('bar') as ?string;

overridden.set('foo', null); // ERROR
