declare class ClassVarianceConflict<out T> {} // ERROR
interface ClassVarianceConflict<in U> {} // ERROR

interface ClassVarianceConflictReversed<out T> {} // ERROR
declare class ClassVarianceConflictReversed<in U> {} // ERROR

declare class ClassVarianceConflictLater<T> {} // ERROR
interface ClassVarianceConflictLater<out U> {}
interface ClassVarianceConflictLater<in V> {} // ERROR

declare class ClassVarianceAgrees<in T> {}
interface ClassVarianceAgrees<in U> {}
interface ClassVarianceAgrees<V> {}

export type { ClassVarianceConflict, ClassVarianceConflictReversed, ClassVarianceConflictLater };
