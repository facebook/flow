// @flow

declare function foo(): Promise<string>;
declare var b: boolean;

// The return hint `number` passed to the generic call causes a SpeculativeError exception
// We should be able to recover from this exception and redo the implicit instantiation
// without the return hint
const x = b ? 3 : (foo().catch(e => {})); // okay
