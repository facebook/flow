declare class Foo {
  i(x: number): void;
  i(x: string): void;
  i<T>(x: Array<T>): void;

  m():void;

  p<T>(x: T): void;
}

declare function test_arg(arg: (x: any) => void): void;
declare function test(arg: () => void): void;

const instance = new Foo();
test_arg(instance.i); // ok
test(instance.m); // ok
test(instance.p); // ok
