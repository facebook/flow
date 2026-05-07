import type {O} from './b';
declare const x: O;
x as number; // error O ~> number
module.exports = x;
