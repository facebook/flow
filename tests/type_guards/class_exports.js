export class C {
  m(x: unknown): x is number {
    return typeof x === 'number';
  }
  os(x: unknown): implies x is number {
    return typeof x === 'number';
  }
}

declare export class D {
  m(x: unknown): x is number;
  os(x: unknown): implies x is number;
}
