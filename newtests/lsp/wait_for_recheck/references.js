// @flow

export type Foo = {baz: string};
export type Bar = {baz: string};

function takesFoo(x: Foo): void {
  x.baz;
}

const y: Bar = {baz: ''};
takesFoo(y);
