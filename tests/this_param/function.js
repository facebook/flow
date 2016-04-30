function fn(this: { x: number }, x: string): string {
  return x + this.x;
}

class A {
  method(): number {
    function k(this: any): number {
      return 5;
    }
    return k();
  }
}

var k = (this: any) => 5;
