// @flow

class B<T> { x: T }
const A = {B};
declare module.exports: A.B<>; // Error: missing-type-arg
