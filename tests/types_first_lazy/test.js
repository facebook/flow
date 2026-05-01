// @flow

const n: string = require('./import-value1'); // error: number ~/~> string
import { f } from './import-value2';
const o = require('./import-value3');
import { type T } from './import-type1';
import { type S } from './import-type2';
import { type O } from './import-type5';

n as T; // error: string ~/~> number
f("") as S; // error: string (argument) ~/~> number, string (return) ~/~> number
o as O; // error: number (property x) <~/~> string
o.y as O; // error: number (property x) <~/~> string
