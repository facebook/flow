import type CycleNodeA from 'CycleNodeA';
import type CycleNodeB from 'CycleNodeB';

declare const record: CycleNodeA;

record.get('items').get(0) as CycleNodeB | void;
