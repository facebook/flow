/* @flow */

function takesNumber(x: number) {}
function takesString(x: string) {}

function num(x: unknown) {
  if (typeof x === 'number') {
    takesString(x); // error
    !x as false; // error: we don't know the truthiness of x
  }
  if (typeof x === 'number' && x) {
    !x as false; // ok
  }
  if (x && typeof x === 'number') {
    !x as false; // ok
  }
}

function str(x: unknown) {
  if (typeof x === 'string') {
    takesNumber(x); // error
    !x as false; // error: we don't know the truthiness of x
  }
  if (typeof x === 'string' && x) {
    !x as false; // ok
  }
  if (x && typeof x === 'string') {
    !x as false; // ok
  }
}

function boolean(x: unknown) {
  if (typeof x === 'boolean') {
    takesString(x); // error
    x as true; // error: we don't know the truthiness of x
  }
  if (typeof x === 'boolean' && x) {
    x as true; // ok
  }
  if (x && typeof x === 'boolean') {
    x as true; // ok
  }
}

function fun(x: unknown) {
  if (typeof x === 'function') {
    takesString(x); // error
  }
}

function obj0(x: unknown) {
  if (typeof x === 'object') {
    takesString(x); // error
  }
}

function obj1(x: unknown) {
  if (Array.isArray(x)) {
    takesString(x); // error
  }
}

function undef(x: unknown) {
  if (typeof x === 'undefined') {
    takesString(x); // error
  }
}

function null_(x: unknown) {
  if (x === null) {
    takesString(x); // error
  }
}

function maybe(x: unknown) {
  if (x == null) {
    takesString(x); // error
  }
}

function true_(x: unknown) {
  if (x === true) {
    takesString(x); // error
  }
}

function false_(x: unknown) {
  if (x === false) {
    takesString(x); // error
  }
}

function obj2(x: unknown) {
  if (typeof x === 'object') {
    x as {+[key: string]: unknown} | null;
    if (x !== null) {
      x['foo'] as string; // error, mixed
    }
  }
}

function obj3(x: unknown) {
  if (typeof x === 'object' && x) {
    x as Object;
  }
  if (x && typeof x === 'object') {
    x as Object;
  }
  if (x != null && typeof x === 'object') {
    x as Object;
  }
  if (x !== null && typeof x === 'object') {
    x as Object;
  }
}

function arr0(x: unknown) {
  if (Array.isArray(x)) {
    takesString(x[0]); // error
  }
}

const loop = (condition: boolean) => {
  let node = 42 as mixed;
  while (condition) {
    if (Array.isArray(node)) {
      node = node[0];
    }
  }
};

{
  declare const x: unknown;
  if (typeof x === 'function') {
    if (typeof x === 'object') {
      x as empty; // OK: Impossible to be both
    }
  }
}
