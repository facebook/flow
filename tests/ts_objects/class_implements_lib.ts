// .ts library exporting an obj-typed shape; a `.js` consumer next door
// declares a class that implements it. The gate is consumer-keyed -- the
// .js consumer must still error.

export type Shape = {a: number; b: string};
