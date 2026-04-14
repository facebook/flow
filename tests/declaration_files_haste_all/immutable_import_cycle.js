import type CycleRecordA from './CycleRecordA';
import type CycleRecordB from './CycleRecordB';

declare const record: CycleRecordA;

record.get('items').get(0) as CycleRecordB | void;
