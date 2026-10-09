interface LibInvariantConflict<in out T> {} // ERROR
interface LibInvariantConflict<out U> {}

interface LibInvariantConflictReversed<in T> {} // ERROR
interface LibInvariantConflictReversed<in out U> {}

interface LibInvariantConflictLater<T> {} // ERROR
interface LibInvariantConflictLater<in out U> {}
interface LibInvariantConflictLater<out V> {}

declare class LibClassInvariantConflict<in out T> {} // ERROR
interface LibClassInvariantConflict<out U> {}

interface LibInvariantAgrees<in out T> {}
interface LibInvariantAgrees<in out U> {}
interface LibInvariantAgrees<V> {}
