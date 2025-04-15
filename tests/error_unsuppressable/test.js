// $FlowFixMe[cannot-resolve-name]
DO_NOT_READ_OR_YOU_WILL_BE_FIRED; // suppressed
// $FlowFixMe[incompatible-type]
const foo: string = true; // not suppressed without speculation
// $FlowFixMe[incompatible-type]
const foo2: string | number = true; // not suppressed with speculation
