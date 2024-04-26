export class C {
  m(x: mixed): x is number {
    return typeof x === 'number';
  }
  os(x: mixed): implies x is number {
    return typeof x === 'number';
  }
}

declare export class D {
  m(x: mixed): x is number;
  os(x: mixed): implies x is number;
}
