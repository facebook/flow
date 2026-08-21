// The lookup happens in TypeScript, so a Flow-declared indexer answers a named access
// here even though the same access is strict from a `.js` file.
import {flowMap} from './flow_declarations';

const flowValueInTs = flowMap.applicationKey;
flowValueInTs as number;
flowValueInTs as string; // ERROR: the indexer value is a number

interface FlowDerivedInTs extends FlowMapBase {}

interface FlowMapBase {
  [name: string]: number;
}

declare const derivedInTs: FlowDerivedInTs;
derivedInTs.inheritedKey as number;

flowMap.writeKey = 1;
flowMap.badWrite = 'no'; // ERROR: writes use the indexer value type

// A named property declared on a later `extends` branch still wins over the indexer.
interface TsExplicitMethods {
  knownMethod(): number;
}

interface TsCombinedWithFlowIndexer extends FlowMapBase, TsExplicitMethods {}

declare const combinedInTs: TsCombinedWithFlowIndexer;
combinedInTs.knownMethod() as number;
