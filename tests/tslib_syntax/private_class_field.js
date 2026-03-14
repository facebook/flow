declare class C {
  #private;
  foo: number;
}

declare const x: C;

x.private; // ERROR
x.foo as number; // OK
