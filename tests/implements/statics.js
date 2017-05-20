/* @noflow */

interface IFoo {
  foo: string;
  static bar: number;
}

class C1 implements IFoo {}
class C2 implements IFoo {
  foo: string;
  static bar: number;
}

interface I {
  x: number;
  static x: number;
  static fn(): number
}

class Impl {
  x: string;
  static x: string;
  static fn(): string {
    return "some string";
  }
};
Impl.x = "another string";

function f(c: I): number {
  return c.constructor.fn();
}

var i = new Impl();
var n: number = f(i);
