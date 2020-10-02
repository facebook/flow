// @flow

// Allowed
export const test1: number = 1;

// Errors
export let test2: number = 2;
export var test3: number = 3;

// Let and var export error
const constVar: number = 1;
let letVar: number = 2;
var varVar: number = 3;

export {constVar, letVar, varVar};
