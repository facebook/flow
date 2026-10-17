// Super chains that produce a new instance type at every level
type Sel<T> = T extends 1 ? Promise<void> : W<T[]>;
type W<T> = interface extends Sel<T> {};
declare function w(): W<2>;

interface I<T> extends I<T[]> {}
declare function i(): I<number>;

async function growing() {
  w(); // ok, the expansion stops at the growing type arguments
  i(); // ok, the expansion stops at the growing type arguments
}
