type T = StringPrefix<'$'>;

component C(x: T) {
  x as T; // OK
  return null;
}
