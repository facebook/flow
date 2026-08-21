// A Flow-declared interface with a string indexer. Named access to it is strict from
// Flow code, but allowed from TypeScript code.
export interface FlowMap {
  [name: string]: number,
}

declare export const flowMap: FlowMap;
