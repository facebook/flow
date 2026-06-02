interface I<in T> {}
class C<out T> implements I<T> {} // Error: out T in a negative position
