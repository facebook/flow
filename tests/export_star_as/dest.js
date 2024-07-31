import {source} from "./test";

var a: number = source.num;
var b: string = source.num; // Error: num ~> string

var c: string = source.str;
var d: number = source.str; // Error: num ~> string

import {foo} from './cycle';
foo as empty; // already errored in cycle
