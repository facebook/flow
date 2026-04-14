import type Immutable from 'immutable';
import type ImportedRecord from './ImportedRecord';

type ListValue = Immutable.List<string>;
type MapValue = Immutable.Map<string, number>;

declare const list: ListValue;
declare const map: MapValue;
declare const record: ImportedRecord;

list.get(0) as string | void;
map.get('x') as number | void;
record.get('groupByTags').get(0) as string | void;
