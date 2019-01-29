// @flow
const x: (?number) => (() => number) = val => {
  if (val == null) {
    return () => 42;
  }
  return () => val; // OK, since val cannot be null
}
