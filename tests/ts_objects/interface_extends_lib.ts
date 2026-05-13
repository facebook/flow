// .ts library declaring an interface that extends an object-typed upper.
// In .ts the relaxation accepts this; the resulting interface type is
// exported and used from a .js consumer next door.

export type Base = {a: number; b: string};
export interface Ext extends Omit<Base, "b"> {
  c: boolean;
}
