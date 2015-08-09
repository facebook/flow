/* @flow */

var x : {[key: string]: string} = {};
var y : {[key: string]: number} = x; // 2 errors, number !~> string & vice versa
var z : {[key: number]: string} = x; // 2 errors, string !~> number & vice versa

var a : {[key: string]: ?string} = {};
var b : {[key: string]: string} = a; // 2 errors (null & undefined)
var c : {[key: string]: ?string} = b; // 2 errors, since c['x'] = null updates b

// 2 errors (number !~> string, string !~> number)
function foo(x: Array<{[key: string]: number}>): Array<{[key: string]: string}> {
  return x;
}

// error, fooBar:string !~> number (x's dictionary)
function foo(
  x: Array<{[key: string]: number}>
): Array<{[key: string]: number, fooBar: string}> {
  return x;
}

function foo(
  x: Array<{[key: string]: mixed}>
): Array<{[key: string]: mixed, fooBar: string}> {
  x[0].fooBar = 123; // error, number !~> string
  return x;
}

// OK, since we assume dictionaries have every key
function foo(x: {[key: string]: number}): {foo: number} {
  return x;
}

// error: foo can't exist in x
function foo(x: {[key: string]: number}): {[key: string]: number, foo: string} {
  return x;
}

// error, some prop in x could be incompatible (covariance)
function foo(x: Array<{[key: string]: number}>): Array<{foo: number}> {
  return x;
}

// error, some prop in return could be incompatible
function foo(x: Array<{foo: number}>): Array<{[key: string]: number}> {
  return x;
}

function foo(x: {bar: string, [key: string]: number}) {
  (x.bar: string);
}

function foo(x: {[key: string]: number}) {
  (x.foo: string); // error
  (x.foo: number);
}
