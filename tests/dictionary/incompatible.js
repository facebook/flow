/* @flow */

var x : {[key: string]: string} = {};
var y : {[key: string]: number} = x; // error, number !~> string
var z : {[key: number]: string} = x; // error, string !~> number

var a : {[key: string]: ?string} = {};
var b : {[key: string]: string} = a; // error
var c : {[key: string]: ?string} = b; // error, since c['x'] = null updates b
