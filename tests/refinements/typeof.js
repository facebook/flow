/* @flow */

function foo(x: boolean | number) {
  if (typeof x === 'boolean') {
    x[0]; // error for boolean, not number
  }
}

function bar(): number {
  var x: null = null;
  if (typeof x === 'object') {
    return x; // error, null
  }
  return 0;
}

/* refining globals */
function fn0() {
  if (typeof BAZ !== 'undefined' && typeof BAZ.stuff === 'function') {
    BAZ.stuff(123);
  }
  BAZ.stuff(123); // error, refinement is gone
}
function fn1() {
  BAZ.stuff; // error, could be undefined
  if (typeof BAZ !== 'undefined' && typeof BAZ.stuff === 'function') {
    BAZ.stuff(123); // ok
    BAZ.stuff(123); // error, refinement is gone
  }
}

function anyfun(x: number | Function): number {
  if (typeof x === 'function') {
    return 0;
  }
  return x; // OK, x refined to `number`
}

function anyobj(x: number | Object): number {
  if (typeof x === 'object') {
    return 0;
  }
  return x; // OK, x refined to `number`
}

function testInvalidValue(x: unknown) {
  if (typeof x === 'foo') {
    // error
    return 0;
  }
}

function testTemplateLiteral(x: string | number) {
  if (typeof x === `string`) {
    return x.length;
  }
}

function testInvalidTemplateLiteral(x: string | number) {
  if (typeof x === `foo`) {
    // error
    return 0;
  }
}

function testClassIsFunction() {
  class Foo {}
  if (typeof Foo === 'function') {
    Foo as empty; // error, Foo is a class
  }
}

function testInstanceIsObject() {
  class Foo {}
  let x = new Foo();
  if (typeof x === 'object') {
    x as empty; // error
  }
}

function testSymbol(x: unknown) {
  if (typeof x === 'symbol') {
    // ok
    x as string; // error
  }
}

function testAnyWithNumber(x: any) {
  if (typeof x === 'number') {
    x as empty; // error number ~> empty
  }
}

function testMixedWithNumber(x: unknown) {
  if (typeof x === 'number') {
    x as number; // OK
    x as empty; // error number ~> empty
  }
}

function testAnyWithNumberPostConditional(x: any) {
  if (typeof x === 'number') {
  }
  x as empty; // ok: x is any again
}

function testAnyWithString(x: any) {
  if (typeof x === 'string') {
    x as empty; // error string ~> empty
  }
}

function testAnyWithBoolean(x: any) {
  if (typeof x === 'boolean') {
    x as empty; // error boolean ~> empty
  }
}

function testMixedWithBoolean(x: unknown) {
  if (typeof x === 'boolean') {
    x as true;
    x as empty; // error boolean ~> empty
  }
}

function testAnyWithSymbol(x: any) {
  if (typeof x === 'symbol') {
    x as empty; // error symbol ~> empty
  }
}

function testPolyClassCtorWithFunction() {
  declare class C<X> {}
  if (typeof C === 'function') {
    C as empty; // error class C ~> empty
  }
}
