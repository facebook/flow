class A {
  constructor(public readonly errorType?: string) {}
}

class B {
  constructor(
    private a?: string,
    protected readonly b?: number,
    readonly c?: boolean,
    public d?: string,
    ordinary?: number
  ) {}
}
