import type Immutable from 'immutable';
import type ImportedHasteRecord from 'ImportedHasteRecord';

type ListValue = Immutable.List<string>;

declare const list: ListValue;
declare const record: ImportedHasteRecord;

list.get(0) as string | void;
record.get('groupByTags').get(0) as string | void;
