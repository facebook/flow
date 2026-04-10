// Property conflict: same-name field
interface PropConflict {
  a: string; // ERROR
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
