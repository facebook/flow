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

interface LibVarianceConflict<out T> {} // ERROR
interface LibVarianceConflict<in U> {}

interface LibVarianceConflictReversed<in T> {} // ERROR
interface LibVarianceConflictReversed<out U> {}

interface LibVarianceConflictLater<T> {} // ERROR
interface LibVarianceConflictLater<out U> {}
interface LibVarianceConflictLater<in V> {}

declare class LibClassVarianceConflict<out T> {} // ERROR
interface LibClassVarianceConflict<in U> {}

interface LibClassVarianceConflictReversed<out T> {}
declare class LibClassVarianceConflictReversed<in U> {} // ERROR

declare class LibClassVarianceConflictLater<T> {} // ERROR
interface LibClassVarianceConflictLater<out U> {}
interface LibClassVarianceConflictLater<in V> {}

interface LibVarianceAgrees<out T> {}
interface LibVarianceAgrees<out U> {}
interface LibVarianceAgrees<V> {}

interface LibVarianceAcrossFiles<out T> {}
declare class LibClassVarianceAcrossFiles<in T> {} // ERROR

