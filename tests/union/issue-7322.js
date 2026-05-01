class Foo {
  noReturn() { }
  returnNoArg(): void {
    return;
  }
  returnUndefined(): void {
    return undefined;
  }
}

type Bar = {
  useFoo(f: Foo): void
};

({} as Bar | {});

const x: Foo = new Foo();

// These should all error
x.noReturn() as number;
x.returnNoArg() as number;
x.returnUndefined() as number;
