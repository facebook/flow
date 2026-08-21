// The exported annotations here are resolved by annotation inference rather than by
// the constraint engine, so they exercise the same indexer deferral on the other side.
import type {TSMap} from './typescript';

declare const tsMap: TSMap;

export const tsRead: typeof tsMap.someKey = 1;
export type TSAlias = typeof tsMap.someKey;

interface FlowMap {
  [name: string]: number,
}
declare const flowMap: FlowMap;

export const flowRead: typeof flowMap.someKey = 1; // ERROR: Flow interfaces keep strict named access
