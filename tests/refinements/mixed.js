/* @flow */

function takesNumber(x: number) {}
function takesString(x: string) {}

function num(x: mixed) {
  if (typeof x === "number") {
    takesString(x); // error
  }
}

function str(x: mixed) {
  if (typeof x === "string") {
    takesNumber(x); // error
  }
}

function bool(x: mixed) {
  if (typeof x === "boolean") {
    takesString(x); // error
  }
}

function fun(x: mixed) {
  if (typeof x === "function") {
    takesString(x); // error
  }
}

function obj0(x: mixed) {
  if (typeof x === "object") {
    takesString(x); // error
  }
}

function obj1(x: mixed) {
  if (Array.isArray(x)) {
    takesString(x); // error
  }
}

function undef(x: mixed) {
  if (typeof x === "undefined") {
    takesString(x); // error
  }
}

function null_(x: mixed) {
  if (x === null) {
    takesString(x); // error
  }
}

function maybe(x: mixed) {
  if (x == null) {
    takesString(x); // error
  }
}

function true_(x: mixed) {
  if (x === true) {
    takesString(x); // error
  }
}

function false_(x: mixed) {
  if (x === false) {
    takesString(x); // error
  }
}
