//@flow

/**
 * foo is a function
 */
function foo(x : number, y : number) : number { return x + y; }

/**
 * Bar is a class
 */
class Bar {}

/**
 * baz is a variable
 */
const baz = 1;

export {foo, Bar, baz};

/** alpha is a number */
export let alpha: number = 0;

/** Beta is a class */
export class Beta {};

/** gamma is a function */
export function gamma() {};

/** delta is a function */
export default function delta() {};
