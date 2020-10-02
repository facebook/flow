// @flow

const num = 42;
export type {num};

function fun(): void {}
export type {fun}

// This one is actually okay, since classes can be used as both values and
// types. However, we should make sure that importers only end up with the type.
class Cls {}
export type {Cls}

// Exported for testing the imported type of Cls
const clsInstance: Cls = new Cls();
export {clsInstance}
