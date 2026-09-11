// The annotated form of the pair, exported. A declaration that spells out a
// bare `unique symbol` and calls the symbol constructor introduces one symbol,
// and its identity comes from the annotation rather than from the call, in the
// checker and in the signature alike. So an importer has to arrive at the same
// symbol the declaring file holds.

export const annotated: unique symbol = Symbol();
export const registered: unique symbol = Symbol.for('registered');

export class C {
  static readonly K: unique symbol = Symbol();
  static readonly L: unique symbol = Symbol.for('L');
}

declare export const table: {
  [annotated]: number,
  [registered]: string,
  [C.K]: boolean,
  [C.L]: null,
};
