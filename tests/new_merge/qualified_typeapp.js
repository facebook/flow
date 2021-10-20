// @flow

class B<T> { x: T }
const A = {B};
// $FlowExpectedError[missing-type-arg]
declare module.exports: A.B<>;
