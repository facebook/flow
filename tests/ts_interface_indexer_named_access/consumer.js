import {tsMap} from './typescript';
import {flowMap as sharedFlowMap} from './flow_declarations';

const tsValue = tsMap.applicationKey;
tsValue as number;
tsValue as string; // ERROR: the indexer value is a number
tsMap['bracketKey'] as number;

interface FlowMap {
  [name: string]: number,
}

declare const flowMap: FlowMap;
const flowValue = flowMap.applicationKey; // ERROR: Flow interfaces keep strict named access
flowValue as number;
flowValue as string; // ERROR: the indexer value is a number

declare const objectMap: {[key: string]: number};
declare const interfaceMap: interface {[key: string]: number};
objectMap.namedKey as number;
interfaceMap.namedKey as number; // ERROR: inline Flow interfaces keep strict named access

interface FlowBaseMap {
  [name: string]: number,
}

interface FlowDerivedMap extends FlowBaseMap {}

declare const flowDerivedMap: FlowDerivedMap;
const inheritedFlowValue = flowDerivedMap.inheritedKey; // ERROR: inherited Flow indexers keep strict named access
inheritedFlowValue as number;
inheritedFlowValue as string; // ERROR: inherited indexer value is a number
flowDerivedMap.writeKey = 1; // ERROR: Flow interfaces keep strict named writes
flowDerivedMap.badWrite = 'no'; // ERROR: missing property and incompatible indexer value

interface RequiredNamedProperty {
  requiredKey: number,
}

flowMap as RequiredNamedProperty; // ERROR: an indexer does not guarantee a required structural property

interface FlowIndexedBase {
  [name: string]: unknown,
}

interface FlowExplicitMethods {
  knownMethod(): number,
}

interface FlowCombined extends FlowIndexedBase, FlowExplicitMethods {}

declare const flowCombined: FlowCombined;
flowCombined.knownMethod() as number;

// The very interface that `ts_consumer.ts` reads through its indexer: from a `.js` file
// neither end is TypeScript, so named access stays strict.
const sharedValue = sharedFlowMap.applicationKey; // ERROR: neither end of the access is TypeScript
sharedValue as number;
