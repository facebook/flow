// Pure TS: object types that a `.js` consumer passes an object with an indexer
// into.

export type AllOptional = {a?: string; b?: string};

export type SomeRequired = {a: string; b?: string};
