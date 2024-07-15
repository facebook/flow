/* This test suite documents a bunch of places where using spread arguments
 * doesn't work, either intentionally or due to us being lazy */

const arr = [1,2,3];  // badly-positioned error

// Object.getPrototypeOf()
Object.getPrototypeOf(...arr); // error

// Object.assign()
const objArr = [ {x: 'string'}, {y: 2}];
const o1 = Object.assign(...objArr); // error
o1.x;
// This is actually fine, we just use array element type
const o2 = Object.assign(({}: {x?: string, y?: number}), ...objArr);
o2.x;
// But this is an error since the array contains non-objects
Object.assign({}, ...[1]); // error
