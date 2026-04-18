// Property conflict: same-name field
interface PropConflict {
  a: string;
}
interface PropConflict {
  a: number;
}

// Tparam mismatch
interface TparamMismatch<T> {
  x: T; // ERROR
}
interface TparamMismatch<T, U> {
  y: U;
}
