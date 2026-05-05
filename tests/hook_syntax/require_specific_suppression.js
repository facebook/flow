hook useH<T>(x: T): [T] { return [x];}
useH as (empty) => unknown; // error
// $FlowFixMe[react-rule-hook-incompatible]
useH as (empty) => unknown; // suppressed
// $FlowFixMe
useH as (empty) => unknown; // error, not suppressed
