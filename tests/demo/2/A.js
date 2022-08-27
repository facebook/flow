/* @providesModule Demo */

class A {
  x: number; // instance field declaration
  constructor(x: number) { this.x = x; }

  getX(): number { return this.x; }

  onLoad(callback: (x: string) => number): number {
    return callback(this.getX());
  }
}

function callback(x: string) { return x.length; }

var a = new A(42);
a.onLoad(callback);
