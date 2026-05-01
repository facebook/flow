import type {O} from './b';
declare var x: O;
x as number; // error O ~> number
module.exports = x;
