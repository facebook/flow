interface VarianceConflict<out T> {} // ERROR
interface VarianceConflict<in U> {} // ERROR

interface VarianceConflictReversed<in T> {} // ERROR
interface VarianceConflictReversed<out U> {} // ERROR

interface VarianceConflictLater<T> {} // ERROR
interface VarianceConflictLater<out U> {}
interface VarianceConflictLater<in V> {} // ERROR

interface VarianceConflictSecond<T, out U> {}
interface VarianceConflictSecond<X, in Y> {} // ERROR

interface VarianceAgrees<out T> {}
interface VarianceAgrees<out U> {}
interface VarianceAgrees<V> {}

export type { VarianceConflict, VarianceConflictReversed, VarianceConflictLater };
