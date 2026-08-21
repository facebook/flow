export interface TSMap {
  [name: string]: number;
}

export declare const tsMap: TSMap;

const internalValue = tsMap.internalKey;
internalValue as number;
internalValue as string; // ERROR: the indexer value is a number

interface TSNumberMap {
  [index: number]: string;
}

declare const tsNumberMap: TSNumberMap;
tsNumberMap.applicationKey; // ERROR: a number indexer does not cover a named property

interface TSBaseMap {
  [name: string]: number;
}

interface TSDerivedMap extends TSBaseMap {}

declare const tsDerivedMap: TSDerivedMap;
const inheritedValue = tsDerivedMap.inheritedKey;
inheritedValue as number;
inheritedValue as string; // ERROR: inherited indexer value is a number
tsDerivedMap.writeKey = 1;
tsDerivedMap.badWrite = 'no'; // ERROR: writes use the inherited indexer value type

interface TSIndexedBase {
  [name: string]: unknown;
}

interface TSExplicitMethods {
  knownMethod(): number;
}

interface TSCombined extends TSIndexedBase, TSExplicitMethods {}

declare const tsCombined: TSCombined;
tsCombined.knownMethod() as number;
