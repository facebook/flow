// Constructor without return type in ambient context
export class A {
  constructor(x: number)
}

// Methods without return type in .d.ts, semi separator
export class B {
  meth1(node: string);
  meth2(node: number)
}

// Methods without return type in .d.ts, no separator
export class C {
  meth1(node: string)
  meth2(node: number)
}
