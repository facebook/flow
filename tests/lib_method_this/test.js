declare const accessor: Accessor<{x: number}, number>;
accessor.get as (this: {x: number}) => number; // OK

class C {
  get(this: number): string { // error: `C` is incompatible with `number`
    return '';
  }
}

interface I<This> {
  get(this: This): void; // error: `I` is incompatible with `This`
}
