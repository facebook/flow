class A {
  constructor(private name: string) {}
}

class B {
  constructor(private readonly x: number) {}
}

class C {
  constructor(
    private a: string,
    protected b: number,
    public c: boolean,
    protected readonly d: string
  ) {}
}

class D {
  constructor(private name: string = "default") {}
}
