class A {
  constructor(
    private name: string, // ERROR
  ) {}
}

class B {
  constructor(
    private readonly x: number, // ERROR
  ) {}
}

class C {
  constructor(
    private a: string, // ERROR
    protected b: number, // ERROR
    public c: boolean, // ERROR
    protected readonly d: string, // ERROR
  ) {}
}

class D {
  constructor(
    private name: string = "default", // ERROR
  ) {}
}
