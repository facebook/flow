// In a .d.ts file every enum is ambient. TypeScript treats an uninitialized
// member of an ambient enum as computed, not a known literal, so we deliberately
// do not auto-number it.
export enum E {
  A,
  B,
}

export enum F {
  A = 1,
  B, // computed in an ambient enum (no auto-numbered literal)
}
