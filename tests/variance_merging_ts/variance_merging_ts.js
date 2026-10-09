interface InvariantVarianceConflict<in out T> {}
interface InvariantVarianceConflict<out U> {} // ERROR

interface InvariantVarianceConflictReversed<in T> {}
interface InvariantVarianceConflictReversed<in out U> {} // ERROR

interface InvariantVarianceConflictLater<T> {}
interface InvariantVarianceConflictLater<in out U> {}
interface InvariantVarianceConflictLater<out V> {} // ERROR

interface InvariantVarianceAgrees<in out T> {}
interface InvariantVarianceAgrees<in out U> {}
interface InvariantVarianceAgrees<V> {}

declare class ClassInvariantConflict<in out T> {}
interface ClassInvariantConflict<out U> {} // ERROR

interface ClassInvariantConflictLater<T> {}
interface ClassInvariantConflictLater<in out U> {} // ERROR
declare class ClassInvariantConflictLater<in V> {}

