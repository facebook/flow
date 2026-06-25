// type-at-pos over a TypeScript enum (a NamespaceT tagged with SymbolEnum). The
// enum name and the enum used as a bare type render as the union of the member
// literals (e.g. `1 | 2`); member access (`Color.Red`) renders the member
// literal. This pins the ty_normalizer rendering of the SymbolEnum case.

enum Color {
//   ^
  Red = 1,
  Green = 2,
}

const r = Color.Red;
//              ^

const x: Color = Color.Red;
//       ^

type T = Color.Red;
//             ^
