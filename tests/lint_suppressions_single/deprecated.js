declare function bar(x: mixed): boolean %checks(x === 1);; // error
// $FlowFixMe[deprecated-type]
declare function baz(x: mixed): boolean %checks(x === 1);; // suppressed
