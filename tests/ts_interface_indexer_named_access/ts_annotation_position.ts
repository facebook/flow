// Same as `annotation_position.js`, but the lookup is written in TypeScript, so the
// Flow-declared indexer answers it. Resolved by annotation inference, not the
// constraint engine.
import {flowMap} from './flow_declarations';

export const flowReadInTs: typeof flowMap.someKey = 1;
export type FlowAliasInTs = typeof flowMap.someKey;
