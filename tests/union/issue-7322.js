// @flow

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

({}: Bar | {});

const x: Foo = new Foo();

// These should all error
(x.noReturn(): number);
(x.returnNoArg(): number);
(x.returnUndefined(): number);
