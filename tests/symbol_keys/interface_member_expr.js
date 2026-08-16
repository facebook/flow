// A member-expression computed key (`[Keys.a]`) whose value is a `unique symbol`
// works within a file too, and distinct symbol keys stay distinct.

declare const Keys: {readonly a: unique symbol, readonly b: unique symbol};

interface I {
  [Keys.a]: number;
  [Keys.b]: string;
}

declare const i: I;
i[Keys.a] as number; // OK
i[Keys.b] as string; // OK
i[Keys.a] as string; // ERROR: number is incompatible with string
